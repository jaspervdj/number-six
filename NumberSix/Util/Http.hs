module NumberSix.Util.Http
    ( httpGet
    , httpScrape
    , urlEncode
    ) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)

import qualified Codec.Binary.UTF8.String as Utf8
import qualified Codec.Binary.Url as Url
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.HTML.TagSoup

import NumberSix.Irc

-- | Perform an HTTP get request and return the response body.
--
httpGet :: String      -- ^ URL
        -> Irc String  -- ^ Response body
httpGet = liftIO . getResponseBody <=< liftIO . simpleHTTP . getRequest

-- | Perform an HTTP get request, and scrape the body using a user-defined
-- function.
--
httpScrape :: String                    -- ^ URL
           -> ([Tag String] -> String)  -- ^ Scrape function
           -> Irc String                -- ^ Result
httpScrape url f = f . parseTags <$> httpGet url

-- | Encode a String to an URL
--
urlEncode :: String -> String
urlEncode = Url.encode . Utf8.encode
