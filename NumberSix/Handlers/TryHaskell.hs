-- | Evaluates Haskell expressions using the TryHaskell API
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.TryHaskell
    ( handler
    ) where

import Data.List (isPrefixOf)
import Control.Applicative ((<$>))
import Data.Either (either)

import Text.JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Codec.Binary.UTF8.String as Utf8

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

eval :: ByteString -> Irc ByteString
eval query = do
    json <- decode . take 200 . Utf8.decode . SB.unpack <$> httpGet url
    return $ either (const complain) id $ resultToEither $ do
        JSObject object <- json
        flip valFromObj object $ if ":t" `SBC.isPrefixOf` query then "type"
                                                                else "result"
  where
    url =  "http://tryhaskell.org/haskell.json?method=eval&expr="
        <> urlEncode query
    complain = "I'm a cybernetic lifeform node. Spare me your rubbish."

handler :: Handler
handler = makeBangHandler "tryhaskell" [">", "!haskell"] eval
