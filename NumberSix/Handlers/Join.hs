-- | Module to join channels
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Join
    ( handler
    ) where

import Control.Monad (forM_)
import Control.Applicative ((<$>))

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler ByteString
handler = makeHandler "join" [autoJoinHook, joinHook]

autoJoinHook :: Irc ByteString ()
autoJoinHook = onCommand "376" $ do
    channels <- getChannels
    forM_ channels $ writeMessage "JOIN" . return

joinHook :: Irc ByteString ()
joinHook = onBangCommand "!join" $ onGod $ do
    (channel, _) <- breakWord <$> getBangCommandText
    writeMessage "JOIN" [channel]
