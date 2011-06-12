{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Tell
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Time
import NumberSix.Util.Sql

handler :: Handler
handler = makeHandlerWith "tell" [storeHook, loadHook] initialize

initialize :: Irc ()
initialize = withSqlRun
    "CREATE TABLE tells (                                              \
    \    id SERIAL,                                                    \
    \    host TEXT, sender TEXT, recipient TEXT, time TEXT, text TEXT  \
    \)"

storeHook :: Irc ()
storeHook = onBangCommand "!tell" $ do
    host <- getHost
    sender <- getSender
    IrcTime time <- getTime
    text' <- getBangCommandText
    let (recipient, text) = breakWord text'
    if recipient ==? sender
        then write "Sigh. The Universe is winning..."
        else do
            _ <- withSql $ \c -> run c
                "INSERT INTO tells (host, sender, recipient, time, text) \
                \VALUES (?, ?, ?, ?, ?)"
                [ toSql host, toSql sender
                , toSql (toLower recipient), toSql time, toSql text ]
            writeReply $ "I'll pass that on when I see " <> recipient <> "."

loadHook :: Irc ()
loadHook = onCommand "PRIVMSG" $ do
    host <- getHost
    recipient <- toLower <$> getSender

    -- Find all messages for the recipient
    messages <- withSql $ \c -> quickQuery' c
        "SELECT sender, time, text FROM tells WHERE host = ? AND recipient = ?"
        [toSql host, toSql recipient]

    case messages of
        [] -> return ()
        ls -> do
            -- Delete the messages
            _ <- withSql $ \c -> run c
                "DELETE FROM tells WHERE host = ? AND recipient = ?"
                [toSql host, toSql recipient]

            -- Print the messages
            forM_ ls $ \[sender, time, text] -> do
                pretty <- prettyTime $ IrcTime $ fromSql time
                -- Use writeChannel to reply in a PM
                writeChannel recipient $
                    fromSql sender <> " (" <> pretty <> "): " <> fromSql text
                sleep 1
