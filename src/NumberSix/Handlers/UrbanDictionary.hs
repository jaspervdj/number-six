-- | Provides term lookup on urbandictionary.com
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.UrbanDictionary
    ( handler
    ) where

import Text.HTML.TagSoup

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

urban :: ByteString -> Irc ByteString
urban query = httpScrape url $ innerText . insideTag "div" .
    dropWhile (~/= TagOpen ("div" :: ByteString) [("class", "definition")])
  where
    url = "http://www.urbandictionary.com/define.php?term=" <> urlEncode query

handler :: UninitiazedHandler
handler = makeBangHandler "urbandictionary" ["!urban"] $ \query -> do
    result <- urban query
    return $ case result of
        "" -> "Not found"
        x  -> x
