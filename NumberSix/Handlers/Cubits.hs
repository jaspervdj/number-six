{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Cubits
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Maybe (fromMaybe)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Redis

handler :: Handler
handler = makeHandler "cubits" cubitsHook

cubitsHook :: Irc ()
cubitsHook = onBangCommand "!cubits" $ do
    args <- SBC.words <$> getBangCommandText
    sender <- getSender
    case args of
        [] -> withCubits id sender
        [nick] -> withCubits id nick
        [nick, n'] -> onGod $ unless (nick == sender) $ do
            let n = read $ SBC.unpack n'
                sn = SBC.pack $ show $ abs n
            writeChannel $ meAction $ if n >= 0
                then "gives " <> nick <> " " <> sn <> " cubits."
                else "takes " <> sn <> " cubits from " <> nick <> "."
            withCubits (+ n) nick

withCubits :: (Integer -> Integer) -> ByteString -> Irc ()
withCubits f nick = withRedis $ \redis -> do
    cubits <- f . fromMaybe 0 <$> getItem redis nick
    setItem redis nick cubits
    writeChannel $ nick <> " has " <> (SBC.pack $ show cubits) <> " cubits."
