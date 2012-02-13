-- | Takes the last item from a GitHub activity feed
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.GitHub
    ( handler
    ) where

import Data.List (find)

import Data.ByteString (ByteString)
import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

gitHub :: ByteString -> Irc ByteString
gitHub query = do
    (text, longUrl) <- httpScrape url $ \tags ->
        let e = insideTag "entry" tags
            text = innerText $ insideTag "title" e
            Just (TagOpen _ a) = find (~== TagOpen ("link" :: ByteString) []) e
            Just url' = lookup "href" a
        in (text, url')
    textAndUrl text longUrl
  where
    url = "http://github.com/" <> urlEncode query <> ".atom"

handler :: Handler
handler = makeBangHandler "github" ["!github"] gitHub
