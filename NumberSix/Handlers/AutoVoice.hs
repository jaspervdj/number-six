-- | Handler to give joining peers voice
--
module NumberSix.Handlers.AutoVoice
    ( handler
    ) where

import NumberSix.Bang
import NumberSix.Irc

handler :: Handler String
handler = makeHandler "autovoice" $ return $ onCommand "JOIN" $ do
    sender <- getSender
    channel <- getChannel
    writeMessage "MODE" [channel, "+v", sender]
