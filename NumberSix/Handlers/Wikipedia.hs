-- | Wikipedia lookup handler
--
module NumberSix.Handlers.Wikipedia
    ( handler
    ) where

import Text.HTML.TagSoup
import Text.Regex.PCRE

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util

wiki :: String -> Irc String String
wiki query = do
    bodyContent <- httpScrape url $
        dropWhile (~/= TagOpen "div" [("id", "bodyContent")])
    let shortContent = innerText $ insideTag "p" bodyContent
    return $ removeNewlines $ if shortContent =~ " may refer to:$"
        then innerText $ insideTag "li" bodyContent
        else shortContent
  where
    url =  "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search="
        <> urlEncode query

handler :: Handler String
handler = makeBangHandler "wikipedia" ["!w", "!wik", "!wiki"] $ \query -> do
    result <- wiki query
    return $ case result of
        "" -> "Not found"
        x  -> x
