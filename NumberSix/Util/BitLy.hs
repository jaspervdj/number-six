-- | Provides URL shortening through the bit.ly API
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.BitLy
    ( shorten
    , textAndUrl
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Util.Http

shorten :: ByteString -> Irc ByteString
shorten query = fromMaybe query <$> httpScrape url getUrl
  where
    getUrl tags = nextTagText tags "url"
    url =  "http://api.bit.ly/v3/shorten?login=jaspervdj"
        <> "&apiKey=R_578fb5b17a40fa1f94669c6cba844df1"
        <> "&longUrl=" <> urlEncode (httpPrefix query)
        <> "&format=xml"

textAndUrl :: ByteString -> ByteString -> Irc ByteString
textAndUrl text url = do
    shortUrl <- shorten url
    return $ text <> " >> " <> shortUrl
