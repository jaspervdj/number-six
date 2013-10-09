{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Tell
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Control.Monad          (forM_)
import           Control.Monad.Trans    (liftIO)
import qualified Database.SQLite.Simple as Sqlite


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Time


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Tell" (map const [storeHook, loadHook]) initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = withDatabase $ \db -> Sqlite.execute_ db
    "CREATE TABLE IF NOT EXISTS tells (                         \
    \    id        INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,  \
    \    host      TEXT                              NOT NULL,  \
    \    channel   TEXT                              NOT NULL,  \
    \    sender    TEXT                              NOT NULL,  \
    \    recipient TEXT                              NOT NULL,  \
    \    time      TEXT                              NOT NULL,  \
    \    text      TEXT                              NOT NULL   \
    \)"


--------------------------------------------------------------------------------
storeHook :: Irc ()
storeHook = onBangCommand "!tell" $ do
    host         <- getHost
    channel      <- getChannel
    sender       <- getSender
    IrcTime time <- liftIO getTime
    text'        <- getBangCommandText
    let (recipient, text) = breakWord text'
    if recipient ==? sender
        then write =<< liftIO randomError
        else do
            withDatabase $ \db -> Sqlite.execute db
                "INSERT INTO tells (host, channel, sender, recipient, \
                \time, text) VALUES (?, ?, ?, ?, ?, ?)"
                (host, channel, sender, (toLower recipient), time, text)
            writeReply $
                "I'll pass that on when I see " <> recipient <> " here."


--------------------------------------------------------------------------------
loadHook :: Irc ()
loadHook = onCommand "PRIVMSG" $ do
    host      <- getHost
    channel   <- getChannel
    recipient <- toLower <$> getSender

    -- Find all messages for the recipient
    messages <- withDatabase $ \db -> Sqlite.query db
        "SELECT sender, time, text FROM tells \
        \WHERE host = ? AND channel = ? AND recipient = ? \
        \ORDER BY id"
        (host, channel, recipient)

    case messages of
        [] -> return ()
        ls -> do
            -- Delete the messages
            withDatabase $ \db -> Sqlite.execute db
                "DELETE FROM tells \
                \WHERE host = ? AND channel = ? AND recipient = ?"
                (host, channel, recipient)

            -- Print the messages
            forM_ ls $ \(sender, time, text) -> do
                pretty <- liftIO $ prettyTime $ IrcTime time
                writeReply $ sender <> " (" <> pretty <> "): " <> text
                sleep 1
