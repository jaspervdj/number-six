-- | Module to join channels
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Join
    ( handler
    ) where

import Control.Monad (forM_, when)
import Control.Applicative ((<$>))

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Bang
import NumberSix.Util

handler :: Handler ByteString
handler = makeHandler "join" [autoJoinHook, joinHook, rejoinHook]

autoJoinHook :: Irc ByteString ()
autoJoinHook = onCommand "376" $ do
    channels <- getChannels
    forM_ channels $ writeMessage "JOIN" . return

joinHook :: Irc ByteString ()
joinHook = onBangCommand "!join" $ onGod $ do
    (channel, _) <- breakWord <$> getBangCommandText
    writeMessage "JOIN" [channel]

rejoinHook :: Irc ByteString ()
rejoinHook = onCommand "KICK" $ do
    (channel : nick' : _) <- getParameters
    myNick <- getNick
    when (nick' ==? myNick) $ do
        sleep 3
        writeMessage "JOIN" [channel]
