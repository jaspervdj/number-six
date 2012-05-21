-- | Finds the title of a webpage
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Title
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import           Text.HTML.TagSoup   (innerText)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
title :: ByteString -> IO ByteString
title query = httpScrape query $ \tags ->
    let title' = innerText $ insideTag "title" tags
    in if B.null title' then "Not now, I'm taking a break."
                        else "Title: " <> title'


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "title" ["!title"] $ liftIO . title
