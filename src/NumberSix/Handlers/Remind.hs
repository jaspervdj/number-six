{-# LANGUAGE OverloadedStrings  #-}
module NumberSix.Handlers.Remind
    ( handler
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           Control.Monad.Trans  (liftIO)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Sql
import           NumberSix.Util.Time


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Remind" (map const [storeHook, loadHook]) initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = createTableUnlessExists "reminds"
    "CREATE TABLE reminders (  \
    \   id SERIAL,             \
    \   host TEXT,             \
    \   channel TEXT,          \
    \   sender TEXT,           \
    \   recipient TEXT,        \
    \   time TEXT,             \
    \   text TEXT              \
    \)"


--------------------------------------------------------------------------------
-- requires a recipient and the text
storeHook :: Irc ()
storeHook = onBangCommand "!remind" $ do
    host         <- getHost
    channel      <- getChannel
    sender       <- getSender
    IrcTime time <- liftIO getTime
    text'        <- getBangCommandText
    let (recipient, text) = breakWord text'
    if recipient ==? sender
        then write =<< liftIO randomError
        else do
            _ <- withSql $ \c -> run c
                "INSERT INTO reminders (host, channel, sender, recipient, \
                \time, text) VALUES (?, ?, ?, ?, ?, ?)"
                [ toSql host, toSql channel, toSql sender
                , toSql (toLower recipient), toSql time, toSql text]
            writeReply $ "Reminder added for " <> recipient


--------------------------------------------------------------------------------
-- provides the sender with the messages stored for him when requested
loadHook :: Irc ()
loadHook = onBangCommand "!reminders" $ do
    host         <- getHost
    channel      <- getChannel
    recipient    <- toLower <$> getSender

    -- Find the reminders for the sender, if any
    reminders <- withSql $ \c -> quickQuery' c
                "SELECT sender, time, text FROM reminders \
                \WHERE host = ? AND channel = ? AND recipient = ? \
                \ORDER BY id"
                [ toSql host, toSql channel, toSql recipient ]

    case reminders of
        [] -> return ()
        rs -> do
            -- delete the messages from the DB
            _ <- withSql $ \c -> run c
                "DELETE FROM reminders \
                \WHERE host = ? AND channel = ? AND recipient = ?"
                [ toSql host, toSql channel, toSql recipient ]

            -- send the reminders to the user in a PRIVMSG
            forM_ rs $ \[sender, time, text] -> do
                pretty <- liftIO $ prettyTime $ IrcTime $ fromSql time
                writeNick recipient $ fromSql sender <> " (" <> pretty <> "): "
                                                     <> fromSql text

