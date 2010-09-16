-- | Add an OP to the channel
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Op
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

handler :: Handler
handler = Handler
    { handlerName = "op"
    , handlerHooks = [opHook, deopHook]
    }

opHook :: Irc ()
opHook = onBangCommand "!op" $ onGod $ do
    nick <- getBangCommandText
    channel <- getChannel
    writeMessage $ makeMessage "MODE" [channel, "+o", nick]

deopHook :: Irc ()
deopHook = onBangCommand "!deop" $ onGod $ do
    nick <- getBangCommandText
    channel <- getChannel
    writeMessage $ makeMessage "MODE" [channel, "-o", nick]
