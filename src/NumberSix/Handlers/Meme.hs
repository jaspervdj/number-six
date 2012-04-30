-- | Handler that allows looking up memes on knowyourmeme.com
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Meme
    ( handler
    ) where

import Text.HTML.TagSoup

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import NumberSix.Handlers.Google (google)
import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Message
import NumberSix.Util.Http
import NumberSix.Util.BitLy

-- | Search for a meme, return the URL of the related page
--
searchMeme :: ByteString -> Irc (Maybe ByteString)
searchMeme query = do
    -- Find the first google result
    url <- google $ "site:knowyourmeme.com " <> query
    -- Check that we have a result as expected
    return $ if "^http://knowyourmeme.com/memes/.*$" `B.isPrefixOf` url
                then Just url
                else Nothing

-- | Get the meme data, from an URL
--
meme :: ByteString -> Irc ByteString
meme url = do
    -- Get the summary
    summary <- httpScrape (url <> ".xml") $ insideTag "summary"
    -- The summary contains HTML, so we need to strip that out again
    let summary' = innerText $ parseTags $ innerText summary  
    textAndUrl (B.take 250 summary') url

handler :: UninitiazedHandler
handler = makeBangHandler "meme" ["!meme"] $ \query -> do
    url <- searchMeme query
    case url of
        Nothing -> return $ query <> " is not a meme"
        Just u' -> meme u'
