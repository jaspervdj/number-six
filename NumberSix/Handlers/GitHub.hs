-- | Takes the last item from a GitHub activity feed
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.GitHub
    ( handler
    ) where

import Data.List (find)

import Text.HTML.TagSoup
import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

gitHub :: ByteString -> Irc ByteString
gitHub query = do
    Just (text, longUrl) <- httpScrape url $ \tags -> do
        let tags' = dropWhile (~/= TagOpen ("entry" :: ByteString) []) tags
        text <- nextTagText tags' "title"
        TagOpen _ attrs <- find (~== TagOpen ("link" :: ByteString) []) tags'
        url' <- lookup "href" attrs
        return (text, url')
    textAndUrl text longUrl
  where
    url = "http://github.com/" <> urlEncode query <> ".atom"

handler :: Handler
handler = makeBangHandler "github" ["!github"] gitHub
