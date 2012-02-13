-- | Handler to express superiority
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Slap
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = makeBangHandler "slap" ["!slap"] $ \nick -> do
    myNick <- getNick
    sender <- getSender
    let bitch = if nick ==? myNick then sender else nick
    return $ meAction $
        "slaps " <> bitch <> " around a bit with a large trout, out of sheer "
                 <> "superiority."
