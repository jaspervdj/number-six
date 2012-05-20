-- | Link to hacker new items (http://news.ycombinator.com)
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.HackerNews
    ( handler
    ) where

import Text.HTML.TagSoup

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

hackerNews :: ByteString -> Irc ByteString
hackerNews query = do
    (title, url) <- httpScrape "news.ycombinator.com" $ \tags ->
        let (_ : TagOpen _ attrs : TagText text : _)
                = dropWhile (~/= TagOpen (B.pack "td") [("class", "title")])
                $ dropWhile (~/= TagText (query <> ".")) tags
            Just href = lookup "href" attrs
            link = if "item" `B.isPrefixOf` href
                        then "http://news.ycombinator.com/" <> href
                        else href
        in (text, link)
    textAndUrl title url

handler :: UninitializedHandler
handler = makeBangHandler "hackernews" ["!hn"] hackerNews
