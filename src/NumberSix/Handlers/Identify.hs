{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Identify
    ( handler
    ) where

import Data.Char (toUpper)

import qualified Data.ByteString.Char8 as B

import NumberSix.Irc

handler :: UninitializedHandler
handler = makeHandlerWith "identify" [] initialize

initialize :: Irc ()
initialize = do
    nick' <- getNick
    realName' <- getRealName
    writeMessage "NICK" [nick']
    writeMessage "USER" [ B.map toUpper nick'
                        , "*", "*", realName'
                        ]
