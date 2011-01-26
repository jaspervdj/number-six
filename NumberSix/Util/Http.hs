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
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Codec.Binary.Url as Url
import Text.HTML.TagSoup
import Text.StringLike (StringLike)
import Network.Curl (curlGetResponse_, respBody, CurlResponse_)
import Network.Curl.Opts

import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Message

-- | Perform an HTTP get request and return the response body. The response body
-- is limited in size, for security reasons.
--
httpGet :: (StringLike s, IrcString s)
        => String   -- ^ URL
        -> Irc s s  -- ^ Response body
httpGet url = do
    response <- liftIO $ curlGetResponse_ (httpPrefix url) curlOptions
    return $ getBody response
  where
    getBody :: IrcString s => CurlResponse_ [(String, String)] ByteString -> s
    getBody = fromByteString . respBody

-- | Perform an HTTP get request, and scrape the body using a user-defined
-- function.
--
httpScrape :: (StringLike s, IrcString s)
           => String          -- ^ URL
           -> ([Tag s] -> a)  -- ^ Scrape function
           -> Irc s a         -- ^ Result
httpScrape url f = f . parseTags <$> httpGet url

-- | Add @"http://"@ to the given URL, if needed
--
httpPrefix :: IrcString s => s -> s
httpPrefix = withIrcByteString $ \url ->
    if "http://" `SBC.isPrefixOf` url || "https://" `SBC.isPrefixOf` url
        then url else "http://" <> url

-- | Some sensible default curl optionsfor an IRC bot
--
curlOptions :: [CurlOption]
curlOptions = [ CurlFollowLocation True
              , CurlTimeoutMS 10000
              , CurlMaxFileSize 128000
              ]

-- | Get the tag list inside an open and closing tag. Supports nested elements.
--
insideTag :: String        -- ^ Tag name
          -> [Tag String]  -- ^ Tag list
          -> [Tag String]  -- ^ Resulting tag list
insideTag tag = inside' 1 . drop 1 . dropWhile (~/= TagOpen tag [])
  where
    inside' :: Int -> [Tag String] -> [Tag String]
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
