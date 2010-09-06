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
    case find (~== TagOpen "a" [("class", "l")]) tags of
        Just (TagOpen _ attrs) -> fromMaybe "Not found" $ lookup "href" attrs
        _ -> "Not found"
  where
    url = "http://www.google.com/search?q=" ++ urlEncode query

handler :: Handler
handler = makeBangHandler "google" "!google" google
