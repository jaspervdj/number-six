module NumberSix.Handlers.Google
    ( handler
    ) where

import Data.List (find)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

google :: String -> Irc String String
google query = httpScrape url $ \tags ->
    let Just (TagOpen _ attrs) =
            find (~== TagOpen "a" [("class", "l")]) tags
        Just t = lookup "href" attrs
    in t
  where
    url = "http://www.google.com/search?q=" <> urlEncode query

handler :: Handler String
handler = makeBangHandler "google" ["!google", "!g"] google
