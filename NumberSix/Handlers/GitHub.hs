-- | Takes the last item from a GitHub activity feed
--
module NumberSix.Handlers.GitHub
    ( handler
    ) where

import Data.List (find)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

gitHub :: String -> Irc String String
gitHub query = do
    (text, longUrl) <- httpScrape url $ \tags ->
        let entry = insideTag "entry" tags
            text = innerText $ insideTag "title" entry
            Just (TagOpen _ attrs) = find (~== TagOpen "link" []) entry
            Just url' = lookup "href" attrs
        in (text, url')
    textAndUrl text longUrl
  where
    url = "http://github.com/" <> urlEncode query <> ".atom"

handler :: Handler String
handler = makeBangHandler "github" ["!github"] gitHub
