{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Sup
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import qualified Data.ByteString as B

import NumberSix.Irc
import NumberSix.Message

handler :: Handler
handler = makeHandler "sup" [supHook]

supHook :: Irc ()
supHook = onCommand "PRIVMSG" $ do
    text <- getMessageText
    expected <- ("sup " <>) <$> getNick
    when (expected `B.isPrefixOf` text) $ do
        sender <- getSender
        write $ "sup " <> sender
