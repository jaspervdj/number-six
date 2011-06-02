-- | Check what's playing on the radio
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.NowPlaying
    ( handler
    ) where

import Text.HTML.TagSoup

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Message
import NumberSix.Util.Http

stubru :: Irc ByteString
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

handler :: Handler
handler = makeBangHandler "nowplaying" ["!nowplaying"] $ \str ->
    case str of
        "stubru" -> stubru
        _        -> return "That's not even a real radio station anyway."
