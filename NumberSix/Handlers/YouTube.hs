-- | Handler that allows looking up video's on YouTube
--
module NumberSix.Handlers.YouTube
    ( handler
    ) where

import Control.Applicative ((<$>))

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy

youTube :: String -> Irc String String
youTube query = do
    -- Find the first entry
    entry <- httpScrape url $ insideTag "entry"

    -- Find the title & URL in the entry
    let title = innerText $ insideTag "title" entry
        [TagOpen _ attrs] = take 1 $
            dropWhile (~/= TagOpen "link" [("rel", "alternate")]) entry
        -- Also drop the '&feature...' part from the URL
        Just link = takeWhile (/= '&') <$> lookup "href" attrs

    -- Format and return
    textAndUrl title link
  where
    url = "http://gdata.youtube.com/feeds/api/videos?q=" <> urlEncode query

handler :: Handler String
handler = makeBangHandler "urbandictionary" ["!youtube"] youTube
