module NumberSix.Handlers.TryHaskell
    ( handler
    ) where

import Control.Applicative

import Text.JSON

import NumberSix.Irc
import NumberSix.Util.Http

eval :: String -> Irc String
eval query = do
    Ok (JSObject object) <- decode <$> httpGet url
    let (Ok result) = valFromObj "result" object
    return result
  where
    url =  "http://tryhaskell.org/haskell.json?method=eval&expr="
        ++ urlEncode query

handler :: Handler
handler = makeHandler "tryhaskell" $ onBangCommand ">" $
    getBangCommandText >>= eval >>= writeChannel
