-- | HTTP and HTML utility functions
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Http
    ( httpGet

    , Doc (..)
    , httpGetNodes
    , httpGetScrape

    , httpPrefix
    , curlOptions

    , urlEncode

    , byTagName
    , byTagNameAttrs
    ) where


--------------------------------------------------------------------------------
import qualified Codec.Binary.Url      as Url
import           Control.Applicative   ((<$>))
import           Control.Monad.Trans   (liftIO)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text)
import           Network.Curl          (curlGetResponse_, respBody, CurlResponse_)
import           Network.Curl.Opts
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Message


--------------------------------------------------------------------------------
-- | Perform an HTTP get request and return the response body. The response body
-- is limited in size, for security reasons.
httpGet :: ByteString     -- ^ URL
        -> IO ByteString  -- ^ Response body
httpGet url = do
    r <- liftIO $ curlGetResponse_ (BC.unpack $ httpPrefix url) curlOptions
    return $ getBody r
  where
    getBody :: CurlResponse_ [(String, String)] ByteString -> ByteString
    getBody = respBody


--------------------------------------------------------------------------------
data Doc = Html | Xml deriving (Show)


--------------------------------------------------------------------------------
httpGetNodes :: Doc -> ByteString -> IO [Node]
httpGetNodes doc url = httpGet url >>= \bs -> case parse source bs of
    Left err   -> error err
    Right doc' -> return $ docContent doc'
  where
    source = BC.unpack url
    parse  = case doc of Html -> parseHTML; Xml -> parseXML


--------------------------------------------------------------------------------
httpGetScrape :: Doc -> ByteString -> (Cursor -> Maybe a)
              -> IO (Maybe a)
httpGetScrape doc url f =
    (>>= f) . fromNodes <$> httpGetNodes doc url


--------------------------------------------------------------------------------
-- | Add @"http://"@ to the given URL, if needed
httpPrefix :: ByteString -> ByteString
httpPrefix url
    | "http://" `B.isPrefixOf` url || "https://" `B.isPrefixOf` url  = url
    | otherwise = "http://" <> url


--------------------------------------------------------------------------------
-- | Some sensible default curl optionsfor an IRC bot
curlOptions :: [CurlOption]
curlOptions = [ CurlFollowLocation True
              , CurlTimeoutMS 10000
              , CurlMaxFileSize 128000
              , CurlUserAgent
                    "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) \
                    \AppleWebKit/534.10 (KHTML, like Gecko) Chrome/8.0.552.237 \
                    \Safari/534.10"
              ]


--------------------------------------------------------------------------------
-- | Encode a ByteString to an URL
urlEncode :: ByteString -> ByteString
urlEncode = BC.pack . Url.encode . B.unpack


--------------------------------------------------------------------------------
byTagName :: Text -> Cursor -> Bool
byTagName t = (== Just t) . tagName . current


--------------------------------------------------------------------------------
byTagNameAttrs :: Text -> [(Text, Text)] -> Cursor -> Bool
byTagNameAttrs name attrs cursor =
    tagName node == Just name &&
    all (uncurry present) attrs
  where
    node              = current cursor
    present key value = getAttribute key node == Just value
