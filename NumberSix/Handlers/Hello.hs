module NumberSix.Handlers.Hello
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang

handler = makeBangHandler "hello" ["!hello"] $ const $ return "O hai!"
