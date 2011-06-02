{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Tell
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Time
import NumberSix.Util.Sql

handler :: Handler ByteString
handler = makeHandlerWith "tell" [storeHook, loadHook] initialize

initialize :: Irc ByteString ()
initialize = withSqlRun
    "CREATE TABLE tells (                                   \
    \    id SERIAL,                                         \
    \    host TEXT, channel TEXT,                           \
    \    sender TEXT, recipient TEXT, time TEXT, text TEXT  \
    \)"

storeHook :: Irc ByteString ()
storeHook = onBangCommand "!tell" $ do
    host <- getHost
    channel <- getChannel
    sender <- getSender
    IrcTime time <- getTime
    text' <- getBangCommandText
    let (recipient, text) = breakWord text'
    if recipient ==? sender
        then write "Sigh. The Universe is winning..."
        else do
            _ <- withSql $ \c -> run c
                "INSERT INTO tells (host, channel, sender, recipient, \
                \time, text) VALUES (?, ?, ?, ?, ?, ?)"
                [ toSql host, toSql channel, toSql sender
                , toSql (toLower recipient), toSql time, toSql text ]
            writeReply $ "I'll pass that on when " <> recipient <> " is here."

loadHook :: Irc ByteString ()
loadHook = onCommand "PRIVMSG" $ do
    host <- getHost
    channel <- getChannel
    recipient <- toLower <$> getSender

    -- Find all messages for the recipient
    messages <- withSql $ \c -> quickQuery' c
        "SELECT sender, time, text FROM tells  \
        \WHERE host = ? AND channel = ? AND recipient = ?"
        [toSql host, toSql channel, toSql recipient]

    case messages of
        [] -> return ()
        ls -> do
            -- Delete the messages
            _ <- withSql $ \c -> run c
                "DELETE FROM tells  \
                \WHERE host = ? AND channel = ? AND recipient = ?"
                [toSql host, toSql channel, toSql recipient]

            -- Print the messages
            forM_ ls $ \[sender, time, text] -> do
                pretty <- prettyTime $ IrcTime $ fromSql time
                writeReply $ fromSql sender <> " (" <> pretty <> "): "
                                            <> fromSql text
                sleep 1
