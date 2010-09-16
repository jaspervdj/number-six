-- | Finds the title of a webpage
--
module NumberSix.Handlers.Title
    ( handler
    ) where

import Data.Maybe (fromMaybe)

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Http

title :: ByteString -> Irc ByteString
title query = httpScrape BrowseHttp query $ \tags ->
    fromMaybe "Not now, I'm taking a break." $ do
        title' <- nextTagText tags "title"
        return $ "Title: " ++ title'

handler :: Handler
handler = makeBangHandler "title" ["!title"] title
