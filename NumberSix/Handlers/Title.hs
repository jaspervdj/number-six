-- | Finds the title of a webpage
--
module NumberSix.Handlers.Title
    ( handler
    ) where

import Text.HTML.TagSoup (innerText)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

title :: String -> Irc String String
title query = httpScrape query $ \tags ->
    let title' = innerText $ insideTag "title" tags
    in if null title' then "Not now, I'm taking a break."
                      else "Title: " <> title'

handler :: Handler String
handler = makeBangHandler "title" ["!title"] title
