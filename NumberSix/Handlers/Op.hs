-- | Add an OP to the channel
--
module NumberSix.Handlers.Op
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang

handler :: Handler String
handler = makeHandler "op" [opHook, deopHook]

opHook :: Irc String ()
opHook = onBangCommand "!op" $ onGod $ do
    nick <- getBangCommandText
    channel <- getChannel
    writeMessage "MODE" [channel, "+o", nick]

deopHook :: Irc String ()
deopHook = onBangCommand "!deop" $ onGod $ do
    nick <- getBangCommandText
    channel <- getChannel
    writeMessage "MODE" [channel, "-o", nick]
