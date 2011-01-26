{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Identify
    ( handler
    ) where

import Control.Monad (forM_)
import Data.Char (toUpper)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc

handler :: Handler ByteString
handler = makeHandlerWith "identify" [joinHook] initialize

initialize :: Irc ByteString ()
initialize = do
    nick' <- getNick
    realName' <- getRealName
    writeMessage "NICK" [nick']
    writeMessage "USER" [ SBC.map toUpper nick'
                        , "*", "*", realName'
                        ]

joinHook :: Irc ByteString ()
joinHook = onCommand "376" $ do
    channels <- getChannels
    forM_ channels $ writeMessage "JOIN" . return
