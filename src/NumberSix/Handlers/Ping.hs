{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Ping
    ( handler
    ) where

import NumberSix.Irc

handler :: UninitiazedHandler
handler = makeHandler "ping" $ return $ onCommand "PING" $ do
    params <- getParameters
    writeMessage "PONG" params
