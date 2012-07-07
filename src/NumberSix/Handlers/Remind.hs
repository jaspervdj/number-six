--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Remind
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Sql
import           NumberSix.Util.Time


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Remind" [const remindHook] initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = createTableUnlessExists "reminds"
    "CREATE TABLE reminds (  \
    \    id SERIAL,          \
    \    host TEXT,          \
    \    sender TEXT,        \
    \    time TEXT,          \
    \    text TEXT           \
    \)"


--------------------------------------------------------------------------------
remindHook :: Irc ()
remindHook = onBangCommand "!remind" $ do
    text <- getBangCommandText
    case text of
        "" -> loadHook
        _  -> storeHook text


--------------------------------------------------------------------------------
-- requires a recipient and the text
storeHook :: ByteString -> Irc ()
storeHook text = do
    host         <- getHost
    sender       <- toLower <$> getSender
    IrcTime time <- liftIO getTime

    _ <- withSql $ \c -> run c
        "INSERT INTO reminds (host, sender, time, text) VALUES (?, ?, ?, ?)"
        [toSql host, toSql sender, toSql time, toSql text]

    writeReply "Noted"


--------------------------------------------------------------------------------
-- | Provides the sender with the messages stored for him when requested
loadHook :: Irc ()
loadHook = do
    host   <- getHost
    sender <- getSender
    let sender' = toLower sender

    -- Find the reminders for the user, if any
    reminds <- withSql $ \c -> quickQuery' c
        "SELECT time, text FROM reminds \
        \WHERE host = ? AND sender = ?  \
        \ORDER BY id"
        [toSql host, toSql sender']

    case reminds of
        [] -> writeNick sender "No reminders"
        _  -> do
            -- delete the messages from the DB
            _ <- withSql $ \c -> run c
                "DELETE FROM reminds WHERE host = ? AND sender = ?"
                [toSql host, toSql sender']

            -- send the reminders to the user in a PRIVMSG
            forM_ reminds $ \[time, text] -> do
                pretty <- liftIO $ prettyTime $ IrcTime $ fromSql time
                writeNick sender $ "(" <> pretty <> "): " <> fromSql text
