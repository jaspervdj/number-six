module NumberSix.Handlers.Google
    ( handler
    ) where

import Data.Maybe (fromMaybe)
import Data.List (find)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Util.Http

google :: String -> Irc String
google query = httpScrape url $ \tags ->
    let Just (TagOpen _ attrs) = find (~== TagOpen "a" [("class", "l")]) tags
        Just t = lookup "href" attrs
        in t
  where
    url = "http://www.google.com/search?q=" ++ urlEncode query

handler :: Handler
handler = makeBangHandler "google" ["!google", "!g"] google
