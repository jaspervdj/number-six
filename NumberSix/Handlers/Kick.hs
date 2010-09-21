-- | Handler to reconnect on kick
--
module NumberSix.Handlers.Kick
    ( handler
    ) where

import Control.Monad (when)

import NumberSix.Bang
import NumberSix.Irc
import NumberSix.Util

handler :: Handler String
handler = makeHandler "kick" $ return $ onCommand "KICK" $ do
    (channel : nick' : _) <- getParameters
    myNick <- getNick
    when (nick' == myNick) $ do
        sleep 3
        writeMessage "JOIN" [channel]
