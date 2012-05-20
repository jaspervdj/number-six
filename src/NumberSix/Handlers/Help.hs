-- | Provide help information
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Help
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang

handler :: UninitializedHandler
handler = makeBangHandler "help" ["!help"] $ const $ return $
    "Documentation can be found at http://github.com/jaspervdj/number-six"
