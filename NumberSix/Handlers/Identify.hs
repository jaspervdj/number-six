{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Identify
    ( handler
    ) where

import Data.Char (toUpper)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc

handler :: Handler ByteString
handler = makeHandlerWith "identify" [] initialize

initialize :: Irc ByteString ()
initialize = do
    nick' <- getNick
    realName' <- getRealName
    writeMessage "NICK" [nick']
    writeMessage "USER" [ SBC.map toUpper nick'
                        , "*", "*", realName'
                        ]
