--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Remind
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Control.Monad          (forM_)
import           Control.Monad.Trans    (liftIO)
import           Data.Text              (Text)
import qualified Database.SQLite.Simple as Sqlite


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Time


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Remind" [const remindHook] initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = withDatabase $ \db -> Sqlite.execute_ db
    "CREATE TABLE IF NOT EXISTS reminds (  \
    \    id     INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,  \
    \    host   TEXT                              NOT NULL,  \
    \    sender TEXT                              NOT NULL,  \
    \    time   TEXT                              NOT NULL,  \
    \    text   TEXT                              NOT NULL  \
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
storeHook :: Text -> Irc ()
storeHook text = do
    host         <- getHost
    sender       <- toLower <$> getSender
    IrcTime time <- liftIO getTime

    withDatabase $ \db -> Sqlite.execute db
        "INSERT INTO reminds (host, sender, time, text) VALUES (?, ?, ?, ?)"
        (host, sender, time, text)

    writeReply "Noted"


--------------------------------------------------------------------------------
-- | Provides the sender with the messages stored for him when requested
loadHook :: Irc ()
loadHook = do
    host   <- getHost
    sender <- getSender
    let sender' = toLower sender

    -- Find the reminders for the user, if any
    reminds <- withDatabase $ \db -> Sqlite.query db
        "SELECT time, text FROM reminds \
        \WHERE host = ? AND sender = ?  \
        \ORDER BY id"
        (host, sender')

    case reminds of
        [] -> writeNick sender "No reminders"
        _  -> do
            -- delete the messages from the DB
            withDatabase $ \db -> Sqlite.execute db
                "DELETE FROM reminds WHERE host = ? AND sender = ?"
                (host, sender')

            -- send the reminders to the user in a PRIVMSG
            forM_ reminds $ \(time, text) -> do
                pretty <- liftIO $ prettyTime $ IrcTime time
                writeNick sender $ "(" <> pretty <> "): " <> text
