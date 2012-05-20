{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Seen
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Sql
import NumberSix.Util.Time

handler :: UninitiazedHandler
handler = makeHandlerWith "seen" (map const [storeHook, loadHook]) initialize

initialize :: Irc ()
initialize = createTableUnlessExists "seen"
    "CREATE TABLE seen (                    \
    \    id SERIAL,                         \
    \    host TEXT, channel TEXT,           \
    \    sender TEXT, time TEXT, text TEXT  \
    \)"

storeHook :: Irc ()
storeHook = onCommand "PRIVMSG" $ do
    host <- getHost
    channel <- getChannel
    sender <- toLower <$> getSender
    IrcTime time <- getTime
    text <- getMessageText
    _ <- withSql $ \c -> do
        r <- quickQuery' c
            "SELECT id FROM seen  \
            \WHERE host = ? AND channel = ? AND sender = ?"
            [toSql host, toSql channel, toSql sender]
        case r of
            -- In the database: update
            [[id']] -> run c
                "UPDATE seen SET time = ?, text = ? WHERE id = ?"
                [toSql time, toSql text, id']
            -- Not yet in the database: insert
            _ -> run c
                "INSERT INTO seen (host, channel, sender, time, text)  \
                \VALUES (?, ?, ?, ?, ?)"
                [ toSql host, toSql channel, toSql sender
                , toSql time, toSql text ]
    return ()

loadHook :: Irc ()
loadHook = onBangCommand "!seen" $ do
    host <- getHost
    channel <- getChannel
    (sender, _) <- first toLower . breakWord <$> getBangCommandText
    r <- withSql $ \c -> quickQuery' c
        "SELECT time, text FROM seen  \
        \WHERE host = ? AND channel = ? AND sender = ?"
        [toSql host, toSql channel, toSql sender]
    case r of
        -- In the database: seen
        [[time, text]] -> do
            pretty <- prettyTime $ IrcTime $ fromSql time
            writeReply $ "I last saw " <> sender <> " " <> pretty
                                       <> " saying: " <> fromSql text
        -- Not yet in the database: not seen
        _ -> writeReply $ "I ain't never seen " <> sender
