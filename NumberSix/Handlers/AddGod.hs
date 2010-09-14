-- | Let users obtain god rights
--
module NumberSix.Handlers.AddGod
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = makeHandler "addgod" $ onBangCommand "!addgod" $ do
    password <- getBangCommandText
    sender <- getSender
    addGod sender password
