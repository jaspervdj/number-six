{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Quote
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans   (liftIO)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isDigit)
import           System.Random         (randomRIO)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Sql


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Quote"
    (map const [addQuoteHook, quoteHook, lastQuoteHook]) initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = createTableUnlessExists "quotes"
    -- A global ID and an ID per channel
    "CREATE TABLE quotes (                   \
    \    id SERIAL,                          \
    \    local_id INT,                       \
    \    host TEXT, channel TEXT, text TEXT  \
    \)"


--------------------------------------------------------------------------------
addQuoteHook :: Irc ()
addQuoteHook = onBangCommand "!addquote" $ do
    text <- getBangCommandText
    host <- getHost
    channel <- getChannel
    localId <- (fmap (+ 1)) getLastId
    _ <- withSql $ \c -> run c
        "INSERT INTO quotes (local_id, host, channel, text) VALUES (?, ?, ?, ?)"
        [toSql localId, toSql host, toSql channel, toSql text]
    write $ "Quote " <> BC.pack (show localId) <> " added"


--------------------------------------------------------------------------------
quoteHook :: Irc ()
quoteHook = onBangCommand "!quote" $ do
    query <- getBangCommandText
    if BC.null query
        -- No query, return a random quote
        then do
            lastId <- getLastId
            r <- liftIO $ randomRIO (1, lastId)
            showQuote r
        else if BC.all isDigit query
            -- A number was given, lookup the quote
            then showQuote (read $ BC.unpack query)
            -- A search term was given, search through quotes
            else do
                qs <- getMatching query
                case qs of
                    [] -> write     =<< liftIO randomError
                    _  -> showQuote =<< liftIO (randomElement qs)
  where
    getMatching :: ByteString -> Irc [Integer]
    getMatching query = do
        host    <- getHost
        channel <- getChannel
        ls <- withSql $ \c -> quickQuery' c
            "SELECT local_id FROM quotes  \
            \WHERE host = ? AND channel = ? AND text ILIKE ?"
            [toSql host, toSql channel, toSql ("%" <> query <> "%")]
        return $ map (\[i] -> fromSql i) ls


--------------------------------------------------------------------------------
lastQuoteHook :: Irc ()
lastQuoteHook = onBangCommand "!lastquote" $ getLastId >>= showQuote


--------------------------------------------------------------------------------
getLastId :: Irc Integer
getLastId = do
    host <- getHost
    channel <- getChannel
    [[r]] <- withSql $ \c -> quickQuery' c
        "SELECT MAX(local_id) FROM quotes  \
        \WHERE host = ? AND channel = ?"
        [toSql host, toSql channel]

    return $ case r of
        SqlNull -> 0
        _       -> fromSql r


--------------------------------------------------------------------------------
showQuote :: Integer -> Irc ()
showQuote n = do
    host    <- getHost
    channel <- getChannel
    [[r]]   <- withSql $ \c -> quickQuery' c
        "SELECT text FROM quotes  \
        \WHERE host = ? AND channel = ? AND local_id = ?"
        [toSql host, toSql channel, toSql n]
    write $ "Quote " <> (BC.pack $ show n) <> ": " <> fromSql r
