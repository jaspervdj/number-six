-- | Provides term lookup on urbandictionary.com
--
module NumberSix.Handlers.UrbanDictionary
    ( handler
    ) where

import Data.Maybe (fromMaybe)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Util.Http

urban :: String -> Irc String
urban query = httpScrape url $ \tags -> fromMaybe "Not found" $ do
    TagText def <- nextTag tags (TagOpen "div" [("class", "definition")])
    return def
  where
    url = "http://www.urbandictionary.com/define.php?term=" ++ urlEncode query

handler :: Handler
handler = makeHandler "urbandictionary" $ onBangCommand "!urban" $
    getBangCommandText >>= urban >>= writeChannel
