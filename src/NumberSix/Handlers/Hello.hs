{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Hello
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang

handler :: UninitializedHandler
handler = makeBangHandler "Hello" ["!hello"] $ const $ return "O hai!"
