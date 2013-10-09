--------------------------------------------------------------------------------
-- | HTTP and HTML utility functions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module NumberSix.Util.Http
    ( http

    , Doc (..)
    , httpNodes
    , httpScrape

    , httpPrefix
    , urlEncode

    , byTagName
    , byTagNameAttrs
    ) where


--------------------------------------------------------------------------------
import qualified Codec.Binary.Url     as Url
import           Control.Applicative  ((<$>))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Conduit as HC
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Message


--------------------------------------------------------------------------------
type RequestOptions = forall m. Monad m => HC.Request m -> HC.Request m


--------------------------------------------------------------------------------
-- | Perform an HTTP get request and return the response body. The response body
-- is limited in size, for security reasons.
http :: Text            -- ^ URL
     -> RequestOptions  -- ^ Set extra request options
     -> IO ByteString   -- ^ Response body
http uri modifyReq = do
    req <- HC.parseUrl uri'
    rsp <- HC.withManager $ \m -> flip HC.httpLbs m $
        modifyReq $ setUserAgent "number-six/0.1.0.0" req
    return $ B.concat $ BL.toChunks $ HC.responseBody rsp
  where
    uri'               = T.unpack $ httpPrefix uri
    setUserAgent ua rq =
        rq {HC.requestHeaders = ("User-Agent", ua) : HC.requestHeaders rq}


--------------------------------------------------------------------------------
data Doc = Html | Xml deriving (Show)


--------------------------------------------------------------------------------
httpNodes :: Doc -> Text -> RequestOptions -> IO [Node]
httpNodes doc url mr = http url mr >>= \bs -> case parse source bs of
    Left err   -> error err
    Right doc' -> return $ docContent doc'
  where
    source = T.unpack url
    parse  = case doc of Html -> parseHTML; Xml -> parseXML


--------------------------------------------------------------------------------
httpScrape :: Doc -> Text -> RequestOptions
           -> (Cursor -> Maybe a) -> IO (Maybe a)
httpScrape doc url mr f = (>>= f) . fromNodes <$> httpNodes doc url mr


--------------------------------------------------------------------------------
-- | Add @"http://"@ to the given URL, if needed
httpPrefix :: Text -> Text
httpPrefix url
    | "http://" `T.isPrefixOf` url || "https://" `T.isPrefixOf` url  = url
    | otherwise = "http://" <> url


--------------------------------------------------------------------------------
-- | Encode a ByteString to an URL
urlEncode :: Text -> Text
urlEncode = T.pack . Url.encode . B.unpack . T.encodeUtf8


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
