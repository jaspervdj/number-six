module NumberSix.Handlers.Shorten
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.BitLy

handler :: Handler String
handler = makeBangHandler "shorten" ["!shorten"] shorten
