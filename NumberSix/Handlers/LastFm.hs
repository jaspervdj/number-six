-- | Get a user's last listened track on last.fm
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.LastFm
    ( handler
    ) where

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

lastFm :: ByteString -> Irc ByteString
lastFm query = do
    Just (text, longUrl) <- httpScrape url $ \tags -> do
        artist <- nextTagText tags "artist"
        name <- nextTagText tags "name"
        url' <- nextTagText tags "url"
        return (query <> " listened to: " <> name <> " by " <> artist , url')
    textAndUrl text longUrl
  where
    url =  "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user="
        <> urlEncode query <> "&api_key=87b8b81da496639cb5a295d78e5f8f4d"

handler :: Handler
handler = makeBangHandler "lastfm" ["!lastfm"] lastFm
