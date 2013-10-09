--------------------------------------------------------------------------------
-- | Main module, containing the 'numberSix' function needed to launch your bot.
{-# LANGUAGE OverloadedStrings #-}
module NumberSix
    ( numberSix
    , numberSixWith
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     ((<$>))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newMVar)
import           Control.Monad           (forM, forM_)
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T
import qualified Database.SQLite.Simple  as Sqlite
import           Prelude                 hiding (catch)
import           System.Environment      (getProgName)


--------------------------------------------------------------------------------
import           NumberSix.Application
import           NumberSix.Handlers
import           NumberSix.Irc
import           NumberSix.Logger
import           NumberSix.Message
import           NumberSix.SandBox


--------------------------------------------------------------------------------
application :: Logger -> [UninitializedHandler] -> IrcConfig -> Application
application logger uninitialized config writer = do
    -- Create the environment
    db   <- newMVar =<< Sqlite.open (ircDatabase config)
    gods <- newMVar []
    let environment = IrcEnvironment config db writer logger gods

    -- Initialize handlers
    handlers' <- fmap catMaybes $ forM uninitialized $
        \h@(UninitializedHandler name _ _) -> do
            let state = IrcState environment
                    (error "NumberSix: message not known yet")
                    (error "Uninitialized handler")
            r <- sandBox logger name (Just 10) $ initializeHandler h state
            case r of
                Nothing -> logger $ "Could not initialize handler " <> name
                Just _  -> logger $ "Initialized handler " <> name

            return r

    -- Return actual application
    return $ \msg ->
        -- Run every handler on the message in a separate thread
        forM_ handlers' $ \h -> do
            let state = IrcState environment msg h
            _ <- forkIO $ sandBox_ logger (handlerName h) (Just 60) $
                runHandler h state
            return ()


--------------------------------------------------------------------------------
-- | Launch a bots and block forever. All default handlers will be activated.
numberSix :: IrcConfig -> IO ()
numberSix = numberSixWith handlers


--------------------------------------------------------------------------------
-- | Launch a bot with given 'SomeHandler's and block forever
numberSixWith :: [UninitializedHandler] -> IrcConfig -> IO ()
numberSixWith handlers' config = do
    logName <- (++ ".log") <$> getProgName
    logger  <- makeLogger logName
    runApplication logger (T.unpack $ ircHost config) (ircPort config) $
        application logger handlers' config
