{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Google
    ( handler
    , google
    ) where

import Data.List (find)

import Data.ByteString (ByteString)
import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

-- | Returns the URL of the first found link
--
google :: ByteString -> Irc ByteString
google query = httpScrape url $ \tags ->
    let Just (TagOpen _ attrs) =
            find (~== TagOpen ("a" :: ByteString) [("class", "l")]) tags
        Just t = lookup "href" attrs
    in t
  where
    url = "http://www.google.com/search?q=" <> urlEncode query

handler :: UninitiazedHandler
handler = makeBangHandler "google" ["!google", "!g"] google
