module NumberSix.Handlers.Google
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.BitLy

handler :: Handler
handler = makeBangHandler "shorten" ["!shorten"] shorten
