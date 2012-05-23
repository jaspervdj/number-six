-- | Get a user's last listened track on last.fm
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.LastFm
    ( handler
    ) where


--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import           Control.Monad.Trans  (liftIO)
import           Data.ByteString      (ByteString)
import           Text.XmlHtml
import qualified Data.Text.Encoding   as T
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.BitLy
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
lastFm :: ByteString -> IO ByteString
lastFm query = do
    result <- httpGetScrape Xml url $ \cursor -> do
        artist <- nodeText . current <$> findRec (byTagName "artist") cursor
        title  <- nodeText . current <$> findRec (byTagName "name")   cursor
        link   <- nodeText . current <$> findRec (byTagName "url")    cursor
        return (T.encodeUtf8 artist, T.encodeUtf8 title, T.encodeUtf8 link)
    case result of
        Just (artist, title, link) -> textAndUrl
            (query <> " listened to: " <> title <> " by " <> artist) link
        _                          -> return "Something went wrong"
  where
    url =  "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user="
        <> urlEncode query <> "&api_key=87b8b81da496639cb5a295d78e5f8f4d"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "lastfm" ["!lastfm"] $ liftIO . lastFm
