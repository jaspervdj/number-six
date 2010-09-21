-- | Link to hacker new items (http://news.ycombinator.com)
--
module NumberSix.Handlers.HackerNews
    ( handler
    ) where

import Text.HTML.TagSoup
import Data.List (isPrefixOf)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

hackerNews :: String -> Irc String String
hackerNews query = do
    (title, url) <- httpScrape "news.ycombinator.com" $ \tags ->
        let (_ : TagOpen _ attrs : TagText text : _)
                = dropWhile (~/= TagOpen "td" [("class", "title")])
                $ dropWhile (~/= TagText (query <> ".")) tags
            Just href = lookup "href" attrs
            link = if "http://" `isPrefixOf` href
                        then href else
                        "http://news.ycombinator.com/" <> href
        in (text, link)
    textAndUrl title url

handler :: Handler String
handler = makeBangHandler "hackernews" ["!hn"] hackerNews
