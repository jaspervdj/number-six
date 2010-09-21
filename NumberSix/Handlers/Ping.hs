{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Ping
    ( handler
    ) where

import Data.ByteString (ByteString)

import NumberSix.Irc

handler :: Handler ByteString
handler = makeHandler "ping" $ return $ onCommand "PING" $ do
    params <- getParameters
    writeMessage "PONG" params
