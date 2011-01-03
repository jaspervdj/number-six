{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Identify
    ( handler
    ) where

import Control.Monad (when, forM_)
import Data.Char (toUpper)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Util

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
joinHook = do
    params <- getParameters
    command' <- getCommand
    channels <- getChannels
    when (command' == "NOTICE" && isCheckIdent params) $ do
        return ()
        -- sleep 10
        -- forM_ channels $ writeMessage "JOIN" . return
  where
    isCheckIdent = any ("Checking Ident" `SBC.isInfixOf`) 
