-- | Handler to reconnect on kick
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Kick
    ( handler
    ) where

import Control.Monad (when)

import NumberSix.Bang
import NumberSix.Irc
import NumberSix.Message
import NumberSix.Util

handler :: Handler
handler = makeHandler "kick" $ onCommand "KICK" $ do
    (channel : nick' : _) <- getParameters
    myNick <- getNick
    when (nick' == myNick) $ do
        sleep 3
        writeMessage $ makeMessage "JOIN" [channel]
