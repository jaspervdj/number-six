-- | Provides term lookup on urbandictionary.com
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.UrbanDictionary
    ( handler
    ) where

import Text.HTML.TagSoup
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

urban :: ByteString -> Irc ByteString
urban query = httpScrape url $
    innerText . takeWhile (~/= TagClose (SBC.pack "div"))
              . dropWhile (~/= TagOpen "div" [(SBC.pack "class", "definition")])
  where
    url = "http://www.urbandictionary.com/define.php?term=" <> urlEncode query

handler :: Handler
handler = makeBangHandler "urbandictionary" ["!urban"] urban
