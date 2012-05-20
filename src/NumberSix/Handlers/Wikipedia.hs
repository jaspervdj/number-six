-- | Wikipedia lookup handler
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Wikipedia
    ( handler
    ) where

import Text.HTML.TagSoup

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http
import NumberSix.Util

wiki :: ByteString -> Irc ByteString
wiki query = do
    bodyContent <- httpScrape url $
        dropWhile (~/= TagOpen ("div" :: ByteString) [("id", "bodyContent")])
    let shortContent = innerText $ insideTag "p" bodyContent
    return $ removeNewlines $ if " may refer to:" `B.isSuffixOf` shortContent
        then innerText $ insideTag "li" bodyContent
        else shortContent
  where
    url =  "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search="
        <> urlEncode query

handler :: UninitializedHandler
handler = makeBangHandler "wikipedia" ["!w", "!wik", "!wiki"] $ \query -> do
    result <- wiki query
    return $ case result of
        "" -> "Not found"
        x  -> x
