module NumberSix.Handlers.Identify
    ( handler
    ) where

import Data.List (isInfixOf)
import Control.Concurrent (threadDelay)
import Control.Monad (when, forM_)
import Data.Char (toUpper)
import Control.Monad.Trans (liftIO)

import Network.IRC

import NumberSix.Irc
import NumberSix.Util

handler :: Handler
handler = makeHandler "identify" $ do
    params <- getParameters
    command <- getCommand
    channels <- getChannels
    when (command == "notice" && isCheckIdent params) $ do
        nick' <- getNick
        realName' <- getRealName
        writeMessage $ nick nick'
        writeMessage $ user (map toUpper nick') "*" "*" realName'
        sleep 10
        forM_ channels $ writeMessage . joinChan
  where
    isCheckIdent = any ("Checking Ident" `isInfixOf`) 
