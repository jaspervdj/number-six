-- | HTTP utility functions
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Http
    ( httpGet
    , httpScrape
    , httpPrefix
    , curlOptions
    , insideTag
    , nextTag
    , nextTagText
    , urlEncode
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Codec.Binary.Url as Url
import Text.HTML.TagSoup
import Network.Curl (curlGetResponse_, respBody, CurlResponse_)
import Network.Curl.Opts

import NumberSix.Message

-- | Perform an HTTP get request and return the response body. The response body
-- is limited in size, for security reasons.
--
httpGet :: ByteString     -- ^ URL
        -> IO ByteString  -- ^ Response body
httpGet url = do
    r <- liftIO $ curlGetResponse_ (BC.unpack $ httpPrefix url) curlOptions
    return $ getBody r
  where
    getBody :: CurlResponse_ [(String, String)] ByteString -> ByteString
    getBody = respBody

-- | Perform an HTTP get request, and scrape the body using a user-defined
-- function.
--
httpScrape :: ByteString               -- ^ URL
           -> ([Tag ByteString] -> a)  -- ^ Scrape function
           -> IO a                     -- ^ Result
httpScrape url f = f . parseTags <$> httpGet url

-- | Add @"http://"@ to the given URL, if needed
--
httpPrefix :: ByteString -> ByteString
httpPrefix url
    | "http://" `B.isPrefixOf` url || "https://" `B.isPrefixOf` url  = url
    | otherwise = "http://" <> url

-- | Some sensible default curl optionsfor an IRC bot
--
curlOptions :: [CurlOption]
curlOptions = [ CurlFollowLocation True
              , CurlTimeoutMS 10000
              , CurlMaxFileSize 128000
              , CurlUserAgent
                    "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) \
                    \AppleWebKit/534.10 (KHTML, like Gecko) Chrome/8.0.552.237 \
                    \Safari/534.10"
              ]

-- | Get the tag list inside an open and closing tag. Supports nested elements.
--
insideTag :: ByteString        -- ^ Tag name
          -> [Tag ByteString]  -- ^ Tag list
          -> [Tag ByteString]  -- ^ Resulting tag list
insideTag tag = inside' 1 . drop 1 . dropWhile (~/= TagOpen tag [])
  where
    inside' :: Int -> [Tag ByteString] -> [Tag ByteString]
    inside' _     [] = []
    inside' stack (x : xs) = case x of
        TagOpen t _ -> 
            if t == tag then x : inside' (stack + 1) xs
                        else consume
        TagClose t ->
            if t /= tag then consume
                        else if stack == 1 then []
                                           else x : inside' (stack - 1) xs
        _ -> consume
      where
        consume = x : inside' stack xs

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
urlEncode = BC.pack . Url.encode . B.unpack
