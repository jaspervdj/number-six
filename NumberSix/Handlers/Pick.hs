-- | Pick a random item from a list
--
module NumberSix.Handlers.Pick
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler String
handler = makeBangHandler "pick" ["!pick", "!who"] $ randomElement . words
