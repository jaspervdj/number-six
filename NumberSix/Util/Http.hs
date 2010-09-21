-- | HTTP utility functions
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Http
    ( httpGet
    , httpScrape
    , httpPrefix
    , curlOptions
    , nextTag
    , nextTagText
    , urlEncode
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Codec.Binary.Url as Url
import Text.HTML.TagSoup
import Network.Curl (curlGetString_)
import Network.Curl.Opts

import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Message

-- | Perform an HTTP get request and return the response body. The response body
-- is limited in size, for security reasons.
--
httpGet :: String             -- ^ URL
        -> Irc String String  -- ^ Response body
httpGet url = liftIO $ fmap snd $ curlGetString_ (httpPrefix url) curlOptions

-- | Perform an HTTP get request, and scrape the body using a user-defined
-- function.
--
httpScrape :: String               -- ^ URL
           -> ([Tag String] -> a)  -- ^ Scrape function
           -> Irc String a         -- ^ Result
httpScrape url f = f . parseTags <$> httpGet url

-- | Add @"http://"@ to the given URL, if needed
--
httpPrefix :: IrcString s => s -> s
httpPrefix = withIrcByteString $ \url ->
    if "http://" `SBC.isPrefixOf` url then url else "http://" <> url

-- | Some sensible default curl optionsfor an IRC bot
--
curlOptions :: [CurlOption]
curlOptions = [ CurlFollowLocation True
              , CurlTimeoutMS 10000
              , CurlMaxFileSize 128000
              ]

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

-- | Encode a ByteString to an URL
--
urlEncode :: IrcString s => s -> s
urlEncode = withIrcByteString $ SBC.pack . Url.encode . SB.unpack
