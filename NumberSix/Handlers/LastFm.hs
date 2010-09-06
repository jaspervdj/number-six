-- | Get a user's last listened track on last.fm
--
module NumberSix.Handlers.LastFm
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Util.Http
import NumberSix.Util.BitLy

lastFm :: String -> Irc String
lastFm query = do
    Just (text, longUrl) <- httpScrape url $ \tags -> do
        artist <- nextTagText tags "artist"
        name <- nextTagText tags "name"
        url' <- nextTagText tags "url"
        return (query ++ " listened to: " ++ name ++ " by " ++ artist , url')
    shortUrl <- shorten longUrl
    return $ text ++ " - " ++ shortUrl
  where
    url =  "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user="
        ++ urlEncode query ++ "&api_key=87b8b81da496639cb5a295d78e5f8f4d"

handler :: Handler
handler = makeBangHandler "lastfm" ["!lastfm"] lastFm
