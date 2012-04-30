-- | Add an OP to the channel
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Op
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Mode

handler :: UninitiazedHandler
handler = makeHandler "op" [opHook, deopHook]

opHook :: Irc ()
opHook = onBangCommand "!op" $ onGod $ do
    nick <- getBangCommandText
    mode "+o" nick

deopHook :: Irc ()
deopHook = onBangCommand "!deop" $ onGod $ do
    nick <- getBangCommandText
    mode "-o" nick
