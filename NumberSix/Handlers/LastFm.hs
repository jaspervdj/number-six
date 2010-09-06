-- | Get a user's last listened track on last.fm
--
module NumberSix.Handlers.LastFm
    ( handler
    ) where

import Data.List (isInfixOf)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Util
import NumberSix.Util.Http

lastFm :: String -> Irc String
lastFm query = httpScrape url $
    trim . innerText
         . takeWhile (~/= TagClose "td")
         . dropWhile (not . isSubjectCell)
         . dropWhile (~/= TagOpen "table" [("id", "recentTracks")])
  where
    url = "http://www.last.fm/user/" ++ urlEncode query
    isSubjectCell (TagOpen _ attrs) = case lookup "class" attrs of
        Nothing -> False
        Just x -> "subjectCell" `isInfixOf` x
    isSubjectCell _ = False

handler :: Handler
handler = makeBangHandler "lastfm" "!lastfm" lastFm
