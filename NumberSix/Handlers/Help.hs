-- | Provide help information
--
module NumberSix.Handlers.Help
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang

handler :: Handler String
handler = makeBangHandler "help" ["!help"] $ const $ return $
    "Documentation can be found at http://github.com/jaspervdj/number-six"
