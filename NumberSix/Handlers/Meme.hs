-- | Handler that allows looking up memes on knowyourmeme.com
--
module NumberSix.Handlers.Meme
    -- ( handler
    -- ) where
    where

import Control.Applicative ((<$>))
import Data.Maybe (listToMaybe, fromMaybe)

import Text.HTML.TagSoup
import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.String

import NumberSix.Handlers.Google
import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util.BitLy
import NumberSix.Util

-- | Search for a meme, return the URL of the related page
--
searchMeme :: String -> Irc String (Maybe String)
searchMeme query = do
    -- Find the first google result
    url <- google $ "site:knowyourmeme.com " ++ query
    -- Check that we have a result as expected
    return $ if url =~ "^http://knowyourmeme.com/memes/.*$"
                then Just url
                else Nothing

-- | Get the meme data, from an URL
--
meme :: String -> Irc String String
meme url = do
    -- Get the summary
    summary <- httpScrape (url ++ ".xml") $ insideTag "summary"
    -- The summary contains HTML, so we need to strip that out again
    let summary' = innerText $ parseTags $ innerText summary  
    textAndUrl (take 250 summary') url

handler :: Handler String
handler = makeBangHandler "meme" ["!meme"] $ \query -> do
    url <- searchMeme query
    case url of
        Nothing -> return $ query ++ " is not a meme"
        Just u' -> meme u'
