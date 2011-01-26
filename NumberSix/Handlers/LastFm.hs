-- | Get a user's last listened track on last.fm
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.LastFm
    ( handler
    ) where

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

lastFm :: String -> Irc String String
lastFm query = do
    (text, longUrl) <- httpScrape url $ \tags ->
        let artist = innerText $ insideTag "artist" tags
            name = innerText $ insideTag "name" tags
            url' = innerText $ insideTag "url" tags
        in (query <> " listened to: " <> name <> " by " <> artist , url')
    textAndUrl text longUrl
  where
    url =  "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user="
        <> urlEncode query <> "&api_key=87b8b81da496639cb5a295d78e5f8f4d"

handler :: Handler String
handler = makeBangHandler "lastfm" ["!lastfm"] lastFm
