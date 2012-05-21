-- | Check what's playing on the radio
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.NowPlaying
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans   (liftIO)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Network.Curl          as Curl
import           Text.HTML.TagSoup


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
stubru :: IO ByteString
stubru = httpScrape url $ \tags ->
    let sel = dropWhile (~/= TagOpen (B.pack "item") [("index", "0")]) tags
        title = innerText $ insideTag "titlename" sel
        artist = innerText $ insideTag "artistname" sel
    in if B.null title || B.null artist
            then "No information found (blame stubru)"
            else title <> " by " <> artist
  where
    url =  "http://internetradio.vrt.be/internetradio_master/"
        <> "productiesysteem2/song_noa/noa_41.xml"


--------------------------------------------------------------------------------
rgrfm :: IO ByteString
rgrfm = do
    bs <- Curl.curlGetResponse_ url options ::
        IO (Curl.CurlResponse_ [(String, String)] ByteString)
    return $ innerText $
        map (\x -> case x of TagOpen "br" [] -> TagText " - "; _ -> x) $
        takeWhile (~/= TagOpen (B.pack "p") [("class", "volgende")]) $
        dropWhile (~/= TagOpen (B.pack "p") [("class", "huidige")]) $
        parseTags $ Curl.respBody bs
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