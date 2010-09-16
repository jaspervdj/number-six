-- | HTTP utility functions
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Http
    ( httpGet
    , httpScrape
    , httpPrefix
    , nextTag
    , nextTagText
    , urlEncode
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Codec.Binary.Url as Url
import Text.HTML.TagSoup
import Network.Curl (curlGetString_)
import Network.Curl.Opts

import NumberSix.Irc
import NumberSix.Message

-- | Perform an HTTP get request and return the response body. The response body
-- is limited to 4096 characters, for security reasons.
--
httpGet :: ByteString      -- ^ URL
        -> Irc ByteString  -- ^ Response body
httpGet url = liftIO $ do
    response <- fmap snd $ curlGetString_ (SBC.unpack url') options
    return $ SB.take 32768 response
  where
    url' = httpPrefix url
    options = [ CurlFollowLocation True
              , CurlTimeoutMS 10000
              , CurlMaxFileSize 128000
              ]

-- | Perform an HTTP get request, and scrape the body using a user-defined
-- function.
--
httpScrape :: ByteString               -- ^ URL
           -> ([Tag ByteString] -> a)  -- ^ Scrape function
           -> Irc a                -- ^ Result
httpScrape url f = f . parseTags <$> httpGet url

-- | Add @"http://"@ to the given URL, if needed
--
httpPrefix :: ByteString -> ByteString
httpPrefix url = if "http://" `SBC.isPrefixOf` url then url
                                                   else "http://" <> url

-- | Get the tag following a certain tag
--
nextTag :: [Tag ByteString] -> Tag ByteString -> Maybe (Tag ByteString)
nextTag tags tag = case dropWhile (~/= tag) tags of
    (_ : x : _) -> Just x
    _ -> Nothing

-- | Get the text chunk following an opening tag with the given name
--
nextTagText :: [Tag ByteString] -> ByteString -> Maybe ByteString
nextTagText tags name = do
    tag <- nextTag tags (TagOpen name [])
    case tag of TagText t -> return t
                _ -> Nothing

-- | Encode a ByteString to an URL
--
urlEncode :: ByteString -> ByteString
urlEncode = SBC.pack . Url.encode . SB.unpack
