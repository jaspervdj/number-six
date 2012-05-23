-- | Check what's playing on the radio
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.NowPlaying
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import qualified Data.Text.Encoding  as T
import qualified Network.Curl        as Curl
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
stubru :: IO ByteString
stubru = do
    result <- httpGetScrape Xml url $ \cursor -> do
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
    -- This code is a monster. But I don't feel bad about it because it's
    -- obviously still way better than the code written by the rgrfm webmaster.
    rsp <- Curl.curlGetResponse_ url options ::
        IO (Curl.CurlResponse_ [(String, String)] ByteString)
    let Right doc   = parseHTML "rgrfm" $ Curl.respBody rsp
        Just cursor = fromNodes $ docContent doc
        Just curr   = findRec (byTagNameAttrs "p" [("class", "huidige")]) cursor
        [x, _, y]   = map nodeText . childNodes $ current curr

    return $ T.encodeUtf8 x <> " - " <> T.encodeUtf8 y
  where
    url     = "http://www.rgrfm.be/core/jajaxfiles/nowplaying.php" 
    options = [Curl.CurlPost True, Curl.CurlPostFields ["ajax=jajax"]] ++
        curlOptions


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "nowplaying" ["!nowplaying"] $ \str -> liftIO $
    case str of
        "stubru" -> stubru
        "rgrfm"  -> rgrfm
        _        -> return "That's not even a real radio station anyway."
