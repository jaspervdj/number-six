module NumberSix.Handlers.Ping
    ( handler
    ) where

import NumberSix.Irc
import Network.IRC

handler :: Handler
handler = makeHandler "ping" $ onCommand "ping" $ do
    params <- getParameters
    writeMessage $ Message Nothing "PONG" params

