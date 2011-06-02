-- | Finds the title of a webpage
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Title
    ( handler
    ) where

import Text.HTML.TagSoup (innerText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

title :: ByteString -> Irc ByteString
title query = httpScrape query $ \tags ->
    let title' = innerText $ insideTag "title" tags
    in if B.null title' then "Not now, I'm taking a break."
                        else "Title: " <> title'

handler :: Handler
handler = makeBangHandler "title" ["!title"] title
