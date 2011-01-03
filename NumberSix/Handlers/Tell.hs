{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Tell
    ( handler
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Time
import NumberSix.Util.Redis

handler :: Handler ByteString
handler = makeHandler "tell" [storeHook, loadHook]

storeHook :: Irc ByteString ()
storeHook = onBangCommand "!tell" $ do
    text <- getBangCommandText
    sender <- getSender
    time <- getTime
    let (recipient, message) = breakWord text
        tell = (sender, time, message)
    withRedis $ \redis -> do
        messages <- fromMaybe [] <$> getItem redis ChannelRealm recipient
        setItem redis ChannelRealm recipient $ messages ++ [tell]
    writeReply $ "I'll pass that on when " <> recipient <> " is here."

loadHook :: Irc ByteString ()
loadHook = onCommand "PRIVMSG" $ withRedis $ \redis -> do
    sender <- getSender
    items <- getItem redis ChannelRealm sender
    case items of
        Nothing -> return ()
        Just l -> do
            deleteItem redis ChannelRealm sender
            forM_ l $ \(from, time, message) -> do
                pretty <- prettyTime time
                writeReply $ from <> " (" <> pretty <> "): " <> message
                sleep 1
