{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Feed
    ( handler
    ) where
--------------------------------------------------------------------------------
import           Control.Applicative     ((<$>))
import           Control.Concurrent      (threadDelay)
import           Control.Monad           (forM, forM_, forever, join)
import           Control.Monad.Trans     (liftIO)
import qualified Data.ByteString.Char8   as B
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time.Clock         (UTCTime)
import qualified Database.SQLite.Simple  as Sqlite
import           Text.Feed.Import        (parseFeedString)
import           Text.Feed.Query         (getFeedItems, getFeedTitle,
                                          getItemLink, getItemPublishDate,
                                          getItemTitle)
import           Text.Feed.Types         (Feed, Item)

--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http
import           NumberSix.Util.Time

--------------------------------------------------------------------------------
delay :: Int
delay = 60 * 1000 -- a minute


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Feed" [const configFeed] initialize

--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = do
    withDatabase $ \db -> Sqlite.execute_ db
        "CREATE TABLE IF NOT EXISTS feeds (                         \
        \   id      INTEGER PRIMARY KEY AUTOINCREMENT   NOT NULL,   \
        \   host    TEXT                                NOT NULL,   \
        \   channel TEXT                                NOT NULL,   \
        \   name    TEXT                                NOT NULL,   \
        \   url     TEXT                                NOT NULL,   \
        \   latest  TEXT                                NOT NULL,   \
        \   UNIQUE (host, channel, name)                            \
        \)"

    -- Spawn feedReader in background
    forkIrc $ feedReader


--------------------------------------------------------------------------------
getFeed :: Text             -- ^ URL
        -> IO (Maybe Feed)  -- ^ Feed on URL
getFeed url = do
    content <- http url id
    return $ parseFeedString $ B.unpack content


--------------------------------------------------------------------------------
newItems :: UTCTime -> Feed -> [Item] -- since when, on this feed
newItems latest feed = Prelude.filter newer $ getFeedItems feed
  where
    newer item = maybe False id $ do
        time <- join $ getItemPublishDate item
        return $ latest < time


--------------------------------------------------------------------------------
layoutItems :: Text -> [Item] -> Irc [Text] -- feed title, items to layout
layoutItems ftitle items = forM items go
  where
    go :: Item -> Irc Text
    go item = maybe (liftIO randomError) (return) $ do
        link   <- T.pack <$> getItemLink item
        ititle <- T.pack <$> getItemTitle item
        return $ "News on " <> ftitle <> ": " <> ititle <> " (" <> link <> ")"


--------------------------------------------------------------------------------
selectFeeds :: Irc [(Text, Text, Text)]
selectFeeds = do
    host         <- getHost
    channel      <- getChannel
    withDatabase $ \db -> Sqlite.query db
        "SELECT name, url, latest FROM feeds WHERE host = ? and channel = ?"
        (host, channel)


--------------------------------------------------------------------------------
feedReader :: Irc ()
feedReader = forever $ do

    -- Gather all feeds we're listening to.
    feeds       <- selectFeeds
    host        <- getHost
    channel     <- getChannel
    IrcTime now <- liftIO getTime

    -- Write items for each feed
    forM_ feeds $ \(name, url, latest) -> do
        feed <- liftIO $ getFeed url
        let time  = read $ T.unpack latest
            news  = maybe [] (newItems time) feed
            title = maybe "" (T.pack . getFeedTitle) feed
        itemlines <- layoutItems title news
        mapM_ write itemlines

        -- Mark the feed as updated if we could access it
        case feed of
             Nothing -> do
                 write $ "Kindly check feed " <> name <> " before I..."
                 write =<< liftIO randomError
             Just _  -> withDatabase $ \db -> Sqlite.execute db 
                "UPDATE TABLE feeds SET latest = ?             \
                \   WHERE host = ? and channel = ? and name = ?"
                (now, host, channel, name)

    -- Sleep and go
    liftIO $ threadDelay delay


--------------------------------------------------------------------------------
configFeed :: Irc ()
configFeed = onBangCommand "!rss" $ do
    text' <- getBangCommandText
    let (command, text) = breakWord text'
    case command of
         "add"    -> let (name, url) = breakWord text in addFeed name url
         "list"   -> listFeeds
         "remove" -> removeFeed text
         _        -> write =<< liftIO randomError


--------------------------------------------------------------------------------
addFeed :: Text -> Text -> Irc ()
addFeed name url = do
    (names, _, _) <- unzip3 <$> selectFeeds
    if name `elem` names
        then write =<< liftIO randomError
        else do
            host         <- getHost
            channel      <- getChannel
            IrcTime time <- liftIO getTime
            withDatabase $ \db -> Sqlite.execute db
                "INSERT INTO feeds (host, channel, name, url, latest) \
                \   VALUES (?, ?, ?, ?, ?)"
                (host, channel, name, url, time)
            writeReply $ "Listening to " <> name <> " on " <> url <> "."


--------------------------------------------------------------------------------
listFeeds :: Irc ()
listFeeds = do
    feeds <- selectFeeds
    writeReply $ "I'm listening to:"
    forM_ feeds $ \(name, url, _) ->
        write $ "  " <> name <> " (" <> url <> ")"


--------------------------------------------------------------------------------
removeFeed :: Text -> Irc ()
removeFeed name = do
    (names, _, _) <- unzip3 <$> selectFeeds
    if not (name `elem` names)
        then write =<< liftIO randomError
        else do
            host    <- getHost
            channel <- getChannel
            withDatabase $ \db -> Sqlite.execute db
                "DELETE FROM feeds WHERE host = ?, channel = ?, name = ?"
                (host, channel, name)
            writeReply $ "Ignoring " <> name <> "."

