-- | Check what's playing on the radio
--
module NumberSix.Handlers.NowPlaying
    ( handler
    ) where

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Http

stubru :: Irc String String
stubru = httpScrape url $ \tags ->
    let selection = dropWhile (~/= TagOpen "item" [("index", "0")]) tags
        title = innerText $ insideTag "titlename" selection
        artist = innerText $ insideTag "artistname" selection
    in if null title || null artist then "No information found (blame stubru)"
                                    else title ++ " by " ++ artist
  where
    url =  "http://internetradio.vrt.be/internetradio_master/"
        ++ "productiesysteem2/song_noa/noa_41.xml"

handler :: Handler String
handler = makeBangHandler "nowplaying" ["!nowplaying"] $ \str ->
    case str of
        "stubru" -> stubru
        _        -> return "That's not even a real radio station anyway."
