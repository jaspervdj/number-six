-- | Pick a random item from a list
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Pick
    ( handler
    ) where

import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = makeBangHandler "pick" ["!pick", "!who"] $ randomElement . B.words
