--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Feed
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Control.Monad          (forM_, forever)
import           Control.Monad.Reader   (ask)
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
import           NumberSix.SandBox
import           NumberSix.Util
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
delay :: Double
delay = 5 * 60  -- 5 minutes


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Feed" [const feedCommands] initialize


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

    -- Spawn checkFeeds in background
    forkIrc $ checkFeeds


--------------------------------------------------------------------------------
getFeed :: Text              -- ^ URL
        -> Irc (Maybe Feed)  -- ^ Feed on URL
getFeed url = do
    logger  <- ircLogger . ircEnvironment <$> ask
    content <- liftIO $ sandBox logger "Feed.getFeed" (Just 30) (http url id)
    return $ parseFeedString =<< fmap B.unpack content


--------------------------------------------------------------------------------
newestItem :: Maybe Text -> Feed -> Maybe Item  -- since when, on this feed
newestItem mLatest feed = do
    newest <- listToMaybe $ getFeedItems feed
    link   <- getItemLink newest
    if Just (T.pack link) == mLatest
        then Nothing
        else return newest


--------------------------------------------------------------------------------
layoutItem :: Text -> Item -> IO Text -- feed title, items to layout
layoutItem fTitle item = do
    let link   = maybe "?" T.pack $ getItemLink item
        iTitle = maybe "?" T.pack $ getItemTitle item
    textAndUrl (fTitle <> ": " <> iTitle) link


--------------------------------------------------------------------------------
selectFeeds :: Irc [(Text, Text, Maybe Text)]
selectFeeds = do
    host <- getHost
    withDatabase $ \db -> Sqlite.query db
        "SELECT channel, url, latest FROM feeds WHERE host = ?"
        (Sqlite.Only host)


--------------------------------------------------------------------------------
checkFeeds :: Irc ()
checkFeeds = forever $ do
    sleep delay

    -- Gather all feeds we're listening to.
    feeds       <- selectFeeds
    host        <- getHost

    -- Write items for each feed
    forM_ feeds $ \(channel, url, mLatest) -> do
        mFeed <- getFeed url
        case mFeed of
            Nothing -> do
                err <- liftIO randomError
                writeChannel channel $ "Feed " <> url <> ": " <> err
            Just feed -> do
                let new   = newestItem mLatest feed
                    title = T.pack $ getFeedTitle feed
                case new of
                    Nothing   -> return ()
                    Just item -> do
                        writeChannel channel =<< liftIO (layoutItem title item)
                        withDatabase $ \db -> Sqlite.execute db
                            "UPDATE feeds SET latest = ?                  \
                            \   WHERE host = ? AND channel = ? AND url = ?"
                            (getItemLink item, host, channel, url)


--------------------------------------------------------------------------------
feedCommands :: Irc ()
feedCommands = onBangCommand "!feed" $ do
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
            writeReply $ "Subscribed to: " <> url


--------------------------------------------------------------------------------
listFeeds :: Irc ()
listFeeds = do
    feeds <- selectFeeds
    case feeds of
        [] -> writeReply $ "I'm not subscribed to any feeds."
        _  -> forM_ feeds $ \(_, url, mLatest) -> write url


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
