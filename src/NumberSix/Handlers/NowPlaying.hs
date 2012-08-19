-- | Check what's playing on the radio
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.NowPlaying
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>))
import           Control.Exception
import           Control.Monad.Trans  (liftIO)
import           Data.ByteString      (ByteString)
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Conduit as HC
import           Prelude hiding (catch)
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
stubru :: IO ByteString
stubru = do
    result <- httpScrape Xml url id $ \cursor -> do
        sel    <- findRec (byTagNameAttrs "item" [("index", "0")]) cursor
        title  <- nodeText . current <$> findRec (byTagName "titlename") sel
        artist <- nodeText . current <$> findRec (byTagName "artistname") sel
        return (T.encodeUtf8 title, T.encodeUtf8 artist)

    return $ case result of
        Just (title, "")     -> title
        Just (title, artist) -> title <> " by " <> artist
        _                    -> "No information found (blame stubru)"
  where
    url = "http://internetradio.vrt.be/internetradio_master/" <>
        "productiesysteem2/song_noa/noa_41.xml"


--------------------------------------------------------------------------------
rgrfm :: IO ByteString
rgrfm = do
    result <- httpScrape Html url mreq $ \cursor -> do
        curr <- findRec (byTagNameAttrs "p" [("class", "huidige")]) cursor
        return $ map nodeText $ childNodes $ current curr

    case result of
        Just [x, _, y] -> return $ T.encodeUtf8 x <> " - " <> T.encodeUtf8 y
        _              -> randomError
  where
    url  = "http://www.rgrfm.be/core/jajaxfiles/nowplaying.php"

    mreq :: Monad m => HC.Request m -> HC.Request m
    mreq = HC.urlEncodedBody [("ajax", "jajax")]


--------------------------------------------------------------------------------
urgent :: IO ByteString
urgent = catch (http url id) (\(SomeException e) -> randomError)
  where
    url  = "http://urgent.fm/nowplaying/livetrack.txt"

--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "NowPlaying" ["!nowplaying"] $ \str -> liftIO $
    case str of
        "stubru" -> stubru
        "rgrfm"  -> rgrfm
        "urgent" -> urgent
        _        -> return "That's not even a real radio station anyway."
