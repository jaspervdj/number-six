-- | Provides URL shortening through the bit.ly API
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.BitLy
    ( shorten
    , textAndUrl
    ) where

import Data.ByteString (ByteString)
import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Util.Http

shorten :: ByteString -> Irc ByteString
shorten query = do
    result <- httpScrape url getUrl
    return $ case result of "" -> url
                            _  -> result
  where
    getUrl = innerText . insideTag "url"
    url =  "http://api.bit.ly/v3/shorten?login=jaspervdj"
        <> "&apiKey=R_578fb5b17a40fa1f94669c6cba844df1"
        <> "&longUrl=" <> urlEncode (httpPrefix query)
        <> "&format=xml"

textAndUrl :: ByteString -> ByteString -> Irc ByteString
textAndUrl text url = do
    shortUrl <- shorten url
    return $ case text of
        "" -> shortUrl
        _  -> text <> " >> " <> shortUrl
