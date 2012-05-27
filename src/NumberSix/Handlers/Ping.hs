{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Ping
    ( handler
    ) where

import NumberSix.Irc

handler :: UninitializedHandler
handler = makeHandler "Ping" $ return $ onCommand "PING" $ do
    params <- getParameters
    writeMessage "PONG" params
