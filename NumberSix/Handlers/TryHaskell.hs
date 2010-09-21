-- | Evaluates Haskell expressions using the TryHaskell API
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.TryHaskell
    ( handler
    ) where

import Data.List (isPrefixOf)
import Control.Applicative ((<$>))

import Text.JSON

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

eval :: String -> Irc String String
eval query = do
    json <- decode <$> httpGet url
    return $ either (const complain) id $ resultToEither $ do
        JSObject object <- json
        flip valFromObj object $ if ":t" `isPrefixOf` query then "type"
                                                            else "result"
  where
    url =  "http://tryhaskell.org/haskell.json?method=eval&expr="
        <> urlEncode query
    complain = "I'm a cybernetic lifeform node. Spare me your rubbish."

handler :: Handler String
handler = makeBangHandler "tryhaskell" [">", "!haskell"] eval
