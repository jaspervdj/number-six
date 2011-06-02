-- | Evaluates Haskell expressions using the TryHaskell API
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.TryHaskell
    ( handler
    ) where

import Control.Applicative ((<$>))

import Data.ByteString (ByteString)
import Text.JSON
import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

eval :: ByteString -> Irc ByteString
eval query = do
    json <- decode . B.unpack <$> httpGet url
    return $ either (const complain) id $ resultToEither $ do
        JSObject object <- json
        flip valFromObj object $ if ":t" `B.isPrefixOf` query then "type"
                                                              else "result"
  where
    url =  "http://tryhaskell.org/haskell.json?method=eval&expr="
        <> urlEncode query
    complain = "I'm a cybernetic lifeform node. Spare me your rubbish."

handler :: Handler
handler = makeBangHandler "tryhaskell" [">", "!haskell"] eval
