-- | HTTP utility functions
--
module NumberSix.Util.Http
    ( HttpMode (..)
    , httpGet
    , httpScrape
    , nextTag
    , nextTagText
    , urlEncode
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)

import qualified Codec.Binary.UTF8.String as Utf8
import qualified Codec.Binary.Url as Url
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Network.Browser (browse, request, setAllowRedirects)
import Text.HTML.TagSoup
import Network.Curl (curlGetString)

import NumberSix.Irc

-- | The HTTP modes available
--
data HttpMode = SimpleHttp  -- ^ Simple HTTP requests
              | BrowseHttp  -- ^ Allows cookies and redirects
              | CurlHttp    -- ^ Use cURL

-- | Perform an HTTP get request and return the response body. The response body
-- is limited to 4096 characters, for security reasons.
--
httpGet :: HttpMode    -- ^ Mode to use
        -> String      -- ^ URL
        -> Irc String  -- ^ Response body
httpGet mode url = liftIO $ do
    response <- case mode of
        SimpleHttp -> getResponseBody =<< simpleHTTP (getRequest url')
        BrowseHttp -> do
            (_, browse') <- browse $ do
                setAllowRedirects True
                request $ getRequest url'
            getResponseBody $ Right browse'
        CurlHttp -> fmap snd $ curlGetString url' []
    return $ take 4096 response
  where
    url' = if "http://" `isPrefixOf` url then url else "http://" ++ url

-- | Perform an HTTP get request, and scrape the body using a user-defined
-- function.
--
httpScrape :: HttpMode             -- ^ Mode to use
           -> String               -- ^ URL
           -> ([Tag String] -> a)  -- ^ Scrape function
           -> Irc a                -- ^ Result
httpScrape mode url f = f . parseTags <$> httpGet mode url

-- | Get the tag following a certain tag
--
nextTag :: [Tag String] -> Tag String -> Maybe (Tag String)
nextTag tags tag = case dropWhile (~/= tag) tags of
    (_ : x : _) -> Just x
    _ -> Nothing

-- | Get the text chunk following an opening tag with the given name
--
nextTagText :: [Tag String] -> String -> Maybe String
nextTagText tags name = do
    tag <- nextTag tags (TagOpen name [])
    case tag of TagText t -> return t
                _ -> Nothing

-- | Encode a String to an URL
--
urlEncode :: String -> String
urlEncode = Url.encode . Utf8.encode
