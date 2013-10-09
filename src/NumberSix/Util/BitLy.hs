--------------------------------------------------------------------------------
-- | Provides URL shortening through the bit.ly API
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.BitLy
    ( shorten
    , textAndUrl
    ) where


--------------------------------------------------------------------------------
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
shorten :: Text -> IO Text
shorten query = do
    result <- httpScrape Xml url id $
        fmap (nodeText . current) . findRec (byTagName "url")
    return $ case result of
        Just x  -> x
        Nothing -> url
  where
    url = "http://api.bit.ly/v3/shorten?login=jaspervdj" <>
        "&apiKey=R_578fb5b17a40fa1f94669c6cba844df1" <>
        "&longUrl=" <> urlEncode (httpPrefix query) <>
        "&format=xml"


--------------------------------------------------------------------------------
textAndUrl :: Text -> Text -> IO Text
textAndUrl text url
    | T.length long <= maxLineLength = return long
    | otherwise                      = do
        shortUrl <- shorten url
        return $ join text shortUrl
  where
    join t u = if T.null t then u else t <> " >> " <> u
    long     = join text url
