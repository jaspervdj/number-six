{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Identify
    ( handler
    ) where

import Data.List (isInfixOf)
import Control.Monad (when, forM_)
import Data.Char (toUpper)

import qualified Data.ByteString.Char8 as SBC

import NumberSix.Bang
import NumberSix.Message
import NumberSix.Irc
import NumberSix.Util

handler :: Handler
handler = makeHandler "identify" $ do
    params <- getParameters
    command' <- getCommand
    channels <- getChannels
    when (command' == "NOTICE" && isCheckIdent params) $ do
        nick' <- getNick
        realName' <- getRealName
        writeMessage $ makeMessage "NICK" [nick']
        writeMessage $ makeMessage "USER" [ SBC.map toUpper nick'
                                          , "*", "*", realName'
                                          ]
        sleep 10
        forM_ channels $ writeMessage . makeMessage "JOIN" . return
  where
    isCheckIdent = any ("Checking Ident" `SBC.isInfixOf`) 
