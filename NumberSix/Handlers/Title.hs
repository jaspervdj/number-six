-- | Finds the title of a webpage
--
module NumberSix.Handlers.Title
    ( handler
    ) where

import Data.Maybe (fromMaybe)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

title :: String -> Irc String String
title query = httpScrape query $ \tags ->
    fromMaybe "Not now, I'm taking a break." $ do
        title' <- nextTagText tags "title"
        return $ "Title: " <> title'

handler :: Handler String
handler = makeBangHandler "title" ["!title"] title
