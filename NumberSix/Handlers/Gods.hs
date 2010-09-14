-- | Ask our cylon for it's gods
--
module NumberSix.Handlers.Gods
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = makeBangHandler "gods" ["!gods"] $ const $ do
    gods <- getGods
    return $ "My gods are " ++ prettyList gods ++ "."
