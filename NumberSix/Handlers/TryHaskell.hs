-- | Evaluates Haskell expressions using the TryHaskell API
--
module NumberSix.Handlers.TryHaskell
    ( handler
    ) where

import Control.Applicative ((<$>))

import Text.JSON

import NumberSix.Irc
import NumberSix.Util.Http

eval :: String -> Irc String
eval query = do
    Ok (JSObject object) <- decode <$> httpGet url
    return $ case valFromObj "result" object of
        Ok result -> result
        Error _ -> "I'm a cybernetic lifeform node. Spare me your rubbish."
  where
    url =  "http://tryhaskell.org/haskell.json?method=eval&expr="
        ++ urlEncode query

handler :: Handler
handler = makeHandler "tryhaskell" $ onBangCommand ">" $
    getBangCommandText >>= eval >>= writeChannel
