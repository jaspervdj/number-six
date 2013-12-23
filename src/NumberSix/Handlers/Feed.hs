{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Feed
    ( handler
    ) where
--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Control.Monad          (forM_)
import           Control.Monad.Trans    (liftIO)
import qualified Data.ByteString.Char8  as B
import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as Sqlite
import           Text.Feed.Import       (parseFeedString)
import           Text.Feed.Query        (getFeedItems, getFeedTitle,
                                         getItemLink, getItemTitle)
import           Text.Feed.Types        (Feed, Item)

--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http

--------------------------------------------------------------------------------
delay :: Double
delay = 15 -- 5 seconds


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
        \   url     TEXT                                NOT NULL,   \
        \   latest  TEXT,                                           \
        \   UNIQUE (host, channel, url)                             \
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
newestItem :: Maybe Text -> Feed -> Maybe Item -- since when, on this feed
newestItem mLatest feed = do
    newest <- listToMaybe $ getFeedItems feed
    link   <- getItemLink newest
    if Just (T.pack link) == mLatest
        then Nothing
        else return newest


--------------------------------------------------------------------------------
layoutItem :: Text -> Item -> Irc Text -- feed title, items to layout
layoutItem ftitle item = maybe (liftIO randomError) (return) $ do
        link   <- T.pack <$> getItemLink item
        ititle <- T.pack <$> getItemTitle item
        return $ "News on " <> ftitle <> ": " <> ititle <> " (" <> link <> ")"


--------------------------------------------------------------------------------
selectFeeds :: Irc [(Text, Text, Maybe Text)]
selectFeeds = do
    host <- getHost
    withDatabase $ \db -> Sqlite.query db
        "SELECT channel, url, latest FROM feeds WHERE host = ?"
        (Sqlite.Only host)


--------------------------------------------------------------------------------
feedReader :: Irc ()
feedReader = do

    sleep delay

    -- Gather all feeds we're listening to.
    feeds       <- selectFeeds
    host        <- getHost

    -- Write items for each feed
    forM_ feeds $ \(channel, url, mLatest) -> do
        mFeed <- liftIO $ getFeed url
        case mFeed of
            Nothing -> do
                writeChannel channel  $ "Kindly check feed " <> url
                       <> " before I..."
                writeChannel channel  =<< liftIO randomError
            Just feed -> do
                let new   = newestItem mLatest feed
                    title = T.pack $ getFeedTitle feed
                case new of
                    Nothing   -> return ()
                    Just item -> do
                        itemline <- layoutItem title item
                        writeChannel channel itemline
                        withDatabase $ \db -> Sqlite.execute db
                            "UPDATE feeds SET latest = ?                  \
                            \   WHERE host = ? AND channel = ? AND url = ?"
                            (getItemLink item, host, channel, url)

    -- Sleep and go
    feedReader


--------------------------------------------------------------------------------
configFeed :: Irc ()
configFeed = onBangCommand "!feed" $ do
    text' <- getBangCommandText
    let (command, text) = breakWord text'
    case command of
         "add"    -> addFeed text
         "list"   -> listFeeds
         "remove" -> removeFeed text
         _        -> write =<< liftIO randomError


--------------------------------------------------------------------------------
addFeed :: Text -> Irc ()
addFeed url = do
    channel <- getChannel
    (channels, urls, _) <- unzip3 <$> selectFeeds
    if (channel, url) `elem` zip channels urls
        then write =<< liftIO randomError
        else do
            host <- getHost
            withDatabase $ \db -> Sqlite.execute db
                "INSERT INTO feeds (host, channel, url) \
                \   VALUES (?, ?, ?)"
                (host, channel, url)
            writeReply $ "Listening to " <> url <> "."


--------------------------------------------------------------------------------
listFeeds :: Irc ()
listFeeds = do
    feeds <- selectFeeds
    writeReply $ "I'm listening to:"
    forM_ feeds $ \(_, url, mLatest) ->
        case mLatest of
             Nothing     -> write $ url
             Just latest -> write $ url <> " (last was: " <> latest <> ")"


--------------------------------------------------------------------------------
removeFeed :: Text -> Irc ()
removeFeed url = do
    channel <- getChannel
    (channels, urls, _) <- unzip3 <$> selectFeeds
    if not $ (channel, url) `elem` zip channels urls
        then write =<< liftIO randomError
        else do
            host <- getHost
            withDatabase $ \db -> Sqlite.execute db
                "DELETE FROM feeds WHERE host = ? AND channel = ? AND url = ?"
                (host, channel, url)
            write $ "Ignoring " <> url <> "."

