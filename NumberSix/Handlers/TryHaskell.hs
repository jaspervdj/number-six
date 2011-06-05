-- | Evaluates Haskell expressions using the TryHaskell API
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.TryHaskell
    ( handler
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Http

-- | Result: type, value
--
data Result = Result ByteString ByteString

instance FromJSON Result where
    parseJSON (Object o) = Result <$> o .: "type" <*> o .: "result"
    parseJSON _          = mzero

eval :: ByteString -> Irc ByteString
eval query = httpGet url >>= \bs -> return $ case parseJsonEither bs of
    Left _ -> "I'm a cybernetic lifeform node. Spare me your rubbish."
    Right (Result t r) -> if ":t" `B.isPrefixOf` query then t else r
  where
    url = "http://tryhaskell.org/haskell.json?method=eval&expr=" <>
        urlEncode query

handler :: Handler
handler = makeBangHandler "tryhaskell" [">", "!haskell"] eval
