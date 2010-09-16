-- | Evaluates Haskell expressions using the TryHaskell API
--
module NumberSix.Handlers.TryHaskell
    ( handler
    ) where

import Data.List (isPrefixOf)
import Control.Applicative ((<$>))
import Data.Either (either)

import Text.JSON

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Http

eval :: ByteString -> Irc ByteString
eval query = do
    json <- decode . take 200 <$> httpGet SimpleHttp url
    return $ either (const complain) id $ resultToEither $ do
        JSObject object <- json
        valFromObj (if ":t" `isPrefixOf` query then "type" else "result") object
  where
    url =  "http://tryhaskell.org/haskell.json?method=eval&expr="
        ++ urlEncode query
    complain = "I'm a cybernetic lifeform node. Spare me your rubbish."

handler :: Handler
handler = makeBangHandler "tryhaskell" [">", "!haskell"] eval
