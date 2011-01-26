{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Sup
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message

handler :: Handler ByteString
handler = makeHandler "sup" [supHook]

supHook :: Irc ByteString ()
supHook = onCommand "PRIVMSG" $ do
    text <- getMessageText
    expected <- ("sup " <>) <$> getNick
    when (expected `SBC.isPrefixOf` text) $ do
        sender <- getSender
        write $ "sup " <> sender
