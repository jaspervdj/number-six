-- | Module to join channels
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Join
    ( handler
    ) where

import Control.Monad (forM_, when)
import Control.Applicative ((<$>))

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: UninitializedHandler
handler = makeHandler "join" [autoJoinHook, joinHook, rejoinHook]

autoJoinHook :: Irc ()
autoJoinHook = onCommand "376" $ do
    channels <- getChannels
    forM_ channels $ writeMessage "JOIN" . return

joinHook :: Irc ()
joinHook = onBangCommand "!join" $ onGod $ do
    (channel, _) <- breakWord <$> getBangCommandText
    writeMessage "JOIN" [channel]

rejoinHook :: Irc ()
rejoinHook = onCommand "KICK" $ do
    (channel : nick' : _) <- getParameters
    myNick <- getNick
    when (nick' ==? myNick) $ do
        sleep 3
        writeMessage "JOIN" [channel]
