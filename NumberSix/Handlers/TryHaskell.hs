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
    json <- decode . take 200 <$> httpGet url
    return $ case json of
        Ok (JSObject object) -> case valFromObj "result" object of
            Ok result -> result
            Error _ -> complain
        _ -> complain
  where
    url =  "http://tryhaskell.org/haskell.json?method=eval&expr="
        ++ urlEncode query
    complain = "I'm a cybernetic lifeform node. Spare me your rubbish."

handler :: Handler
handler = makeBangHandler "tryhaskell" ">" $ fmap return . eval
