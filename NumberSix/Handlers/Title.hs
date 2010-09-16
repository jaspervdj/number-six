-- | Finds the title of a webpage
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Title
    ( handler
    ) where

import Data.Maybe (fromMaybe)

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

title :: ByteString -> Irc ByteString
title query = httpScrape query $ \tags ->
    fromMaybe "Not now, I'm taking a break." $ do
        title' <- nextTagText tags "title"
        return $ "Title: " <> title'

handler :: Handler
handler = makeBangHandler "title" ["!title"] title
