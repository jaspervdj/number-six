--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Quote
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans    (liftIO)
import           Data.Char              (isDigit)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as Sqlite
import           System.Random          (randomRIO)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Quote"
    (map const [addQuoteHook, quoteHook, lastQuoteHook]) initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = withDatabase $ \db -> Sqlite.execute_ db
    -- A global ID and an ID per channel
    "CREATE TABLE IF NOT EXISTS quotes (     \
    \    id       INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,  \
    \    local_id INTEGER                           NOT NULL,  \
    \    host     TEXT                              NOT NULL,  \
    \    channel  TEXT                              NOT NULL,  \
    \    text     TEXT                              NOT NULL   \
    \)"


--------------------------------------------------------------------------------
addQuoteHook :: Irc ()
addQuoteHook = onBangCommand "!addquote" $ do
    text    <- getBangCommandText
    host    <- getHost
    channel <- getChannel
    localId <- (fmap (+ 1)) getLastId
    withDatabase $ \db -> Sqlite.execute db
        "INSERT INTO quotes (local_id, host, channel, text) VALUES (?, ?, ?, ?)"
        (localId, host, channel, text)
    write $ "Quote " <> T.pack (show localId) <> " added"


--------------------------------------------------------------------------------
quoteHook :: Irc ()
quoteHook = onBangCommand "!quote" $ do
    query <- getBangCommandText
    if T.null query
        -- No query, return a random quote
        then do
            lastId <- getLastId
            r <- liftIO $ randomRIO (1, lastId)
            showQuote r
        else if T.all isDigit query
            -- A number was given, lookup the quote
            then showQuote (read $ T.unpack query)
            -- A search term was given, search through quotes
            else do
                qs <- getMatching query
                case qs of
                    [] -> write     =<< liftIO randomError
                    _  -> showQuote =<< liftIO (randomElement qs)
  where
    getMatching :: Text -> Irc [Integer]
    getMatching query = do
        host    <- getHost
        channel <- getChannel
        ls      <- withDatabase $ \db -> Sqlite.query db
            "SELECT local_id FROM quotes  \
            \WHERE host = ? AND channel = ? AND LOWER(text) LIKE ?"
            (host, channel, "%" <> toLower query <> "%")
        return [i | Sqlite.Only i <- ls]


--------------------------------------------------------------------------------
lastQuoteHook :: Irc ()
lastQuoteHook = onBangCommand "!lastquote" $ getLastId >>= showQuote


--------------------------------------------------------------------------------
getLastId :: Irc Integer
getLastId = do
    host    <- getHost
    channel <- getChannel
    rs      <- withDatabase $ \db -> Sqlite.query db
        "SELECT MAX(local_id) FROM quotes  \
        \WHERE host = ? AND channel = ?"
        (host, channel)

    return $ case rs of
        [Sqlite.Only (Just r)] -> r
        _                      -> 0


--------------------------------------------------------------------------------
showQuote :: Integer -> Irc ()
showQuote n = do
    host            <- getHost
    channel         <- getChannel
    [Sqlite.Only r] <- withDatabase $ \db -> Sqlite.query db
        "SELECT text FROM quotes  \
        \WHERE host = ? AND channel = ? AND local_id = ?"
        (host, channel, n)
    write $ "Quote " <> (T.pack $ show n) <> ": " <> r
