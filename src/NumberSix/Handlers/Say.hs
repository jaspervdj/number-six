-- | Handler that allows gods to say arbitrary text
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Say
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: UninitializedHandler
handler = makeHandler "Say" $ return $ onBangCommand "!say" $ onGod $
    uncurry writeChannel . breakWord =<< getBangCommandText
