-- | Provides term lookup on urbandictionary.com
--
module NumberSix.Handlers.UrbanDictionary
    ( handler
    ) where

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Http

urban :: String -> Irc String
urban query = httpScrape url $
    innerText . takeWhile (~/= TagClose "div")
              . dropWhile (~/= TagOpen "div" [("class", "definition")])
  where
    url = "http://www.urbandictionary.com/define.php?term=" ++ urlEncode query

handler :: Handler
handler = makeBangHandler "urbandictionary" ["!urban"] urban
