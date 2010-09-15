-- | Add an OP to the channel
--
module NumberSix.Handlers.Op
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Http
import Network.IRC

handler :: Handler
handler = Handler
    { handlerName = "op"
    , handlerHooks = [opHook, deopHook]
    }

opHook :: Irc ()
opHook = onBangCommand "!op" $ onGod $ do
    nick <- getBangCommandText
    channel <- getChannel
    writeMessage $ Message Nothing "MODE" [channel, "+o", nick]

deopHook :: Irc ()
deopHook = onBangCommand "!deop" $ onGod $ do
    nick <- getBangCommandText
    channel <- getChannel
    writeMessage $ Message Nothing "MODE" [channel, "-o", nick]
