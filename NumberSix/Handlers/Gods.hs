-- | Ask our cylon for it's gods
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Gods
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = makeBangHandler "gods" ["!gods"] $ const $ do
    gods <- getGods
    return $ "My gods are " <> prettyList gods <> "."
