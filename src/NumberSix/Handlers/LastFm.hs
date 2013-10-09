--------------------------------------------------------------------------------
-- | Get a user's last listened track on last.fm
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.LastFm
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>))
import           Control.Monad.Trans  (liftIO)
import           Data.Text            (Text)
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
lastFm :: Text -> IO Text
lastFm query = do
    result <- httpScrape Xml url id $ \cursor -> do
        artist <- nodeText . current <$> findRec (byTagName "artist") cursor
        title  <- nodeText . current <$> findRec (byTagName "name")   cursor
        link   <- nodeText . current <$> findRec (byTagName "url")    cursor
        return (artist, title, link)
    case result of
        Just (artist, title, link) -> textAndUrl
            (query <> " listened to: " <> title <> " by " <> artist) link
        _                          -> randomError
  where
    url =  "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user="
        <> urlEncode query <> "&api_key=87b8b81da496639cb5a295d78e5f8f4d"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "LastFm" ["!lastfm"] $ liftIO . lastFm
