-- | Handler to rejoin channel on kick
--
module NumberSix.Handlers.Rejoin
    ( handler
    ) where

import Control.Monad (when)

import NumberSix.Bang
import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Util

handler :: Handler String
handler = makeHandler "rejoin" $ return $ onCommand "KICK" $ do
    (channel : nick' : _) <- getParameters
    myNick <- getNick
    when (nick' ==? myNick) $ do
        sleep 3
        writeMessage "JOIN" [channel]
