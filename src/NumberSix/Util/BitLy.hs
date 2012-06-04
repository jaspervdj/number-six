-- | Provides URL shortening through the bit.ly API
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.BitLy
    ( shorten
    , textAndUrl
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString     (ByteString)
import qualified Data.Text.Encoding  as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
shorten :: ByteString -> IO ByteString
shorten query = do
    result <- httpScrape Xml url id $
        fmap (nodeText . current) . findRec (byTagName "url")
    return $ case result of
        Just x  -> T.encodeUtf8 x
        Nothing -> url
  where
    url = "http://api.bit.ly/v3/shorten?login=jaspervdj" <>
        "&apiKey=R_578fb5b17a40fa1f94669c6cba844df1" <>
        "&longUrl=" <> urlEncode (httpPrefix query) <>
        "&format=xml"


--------------------------------------------------------------------------------
textAndUrl :: ByteString -> ByteString -> IO ByteString
textAndUrl text url = do
    shortUrl <- shorten url
    return $ case text of
        "" -> shortUrl
        _  -> text <> " >> " <> shortUrl
