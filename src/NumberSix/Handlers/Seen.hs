--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Seen
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Control.Arrow          (first)
import           Control.Monad.Trans    (liftIO)
import qualified Database.SQLite.Simple as Sqlite


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Time


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Seen" (map const [storeHook, loadHook]) initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = withDatabase $ \db -> Sqlite.execute_ db
    "CREATE TABLE IF NOT EXISTS seen (                        \
    \    id      INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,  \
    \    host    TEXT                              NOT NULL,  \
    \    channel TEXT                              NOT NULL,  \
    \    sender  TEXT                              NOT NULL,  \
    \    time    TEXT                              NOT NULL,  \
    \    text    TEXT                              NOT NULL   \
    \)"


--------------------------------------------------------------------------------
storeHook :: Irc ()
storeHook = onCommand "PRIVMSG" $ do
    host         <- getHost
    channel      <- getChannel
    sender       <- toLower <$> getSender
    IrcTime time <- liftIO getTime
    text         <- getMessageText
    withDatabase $ \db -> do
        r <- Sqlite.query db
            "SELECT id FROM seen  \
            \WHERE host = ? AND channel = ? AND sender = ?"
            (host, channel, sender)
        case r of
            -- In the database: update
            [Sqlite.Only id'] -> Sqlite.execute db
                "UPDATE seen SET time = ?, text = ? WHERE id = ?"
                (time, text, id' :: Integer)
            -- Not yet in the database: insert
            _ -> Sqlite.execute db
                "INSERT INTO seen (host, channel, sender, time, text)  \
                \VALUES (?, ?, ?, ?, ?)"
                (host, channel, sender, time, text)
    return ()


--------------------------------------------------------------------------------
loadHook :: Irc ()
loadHook = onBangCommand "!seen" $ do
    host        <- getHost
    channel     <- getChannel
    (sender, _) <- first toLower . breakWord <$> getBangCommandText
    r           <- withDatabase $ \db -> Sqlite.query db
        "SELECT time, text FROM seen  \
        \WHERE host = ? AND channel = ? AND sender = ?"
        (host, channel, sender)
    case r of
        -- In the database: seen
        [(time, text)] -> do
            pretty <- liftIO $ prettyTime $ IrcTime time
            writeReply $ "I last saw " <> sender <> " " <> pretty
                                       <> " saying: " <> text
        -- Not yet in the database: not seen
        _ -> writeReply $ "I ain't never seen " <> sender
