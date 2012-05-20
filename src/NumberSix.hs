-- | Main module, containing the 'numberSix' function needed to launch your bot.
{-# LANGUAGE OverloadedStrings #-}
module NumberSix
    ( numberSix
    , numberSixWith
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Chan.Strict (readChan, writeChan)
import           Control.Concurrent.MVar        (newMVar)
import           Control.Monad                  (forever, forM, forM_)
import           Data.Maybe                     (catMaybes)
import           Prelude                        hiding (catch)
import qualified Data.ByteString.Char8          as SBC
import           System.Environment             (getProgName)


--------------------------------------------------------------------------------
import           NumberSix.ExponentialBackoff
import           NumberSix.Handlers
import           NumberSix.Irc
import           NumberSix.Logger
import           NumberSix.Message
import           NumberSix.Message.Decode
import           NumberSix.Message.Encode
import           NumberSix.SandBox
import           NumberSix.Socket


--------------------------------------------------------------------------------
-- | Run a single IRC connection
irc :: Logger                  -- ^ Logger
    -> [UninitializedHandler]  -- ^ Handlers
    -> IrcConfig               -- ^ Configuration
    -> IO ()
irc logger uninitialized config = withConnection' $ \inChan outChan -> do
    -- Create the environment
    gods <- newMVar []
    let environment = IrcEnvironment config (writer outChan) logger gods

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

    forever $ handleLine environment handlers' inChan
  where
    withConnection' =
        withConnection (SBC.unpack $ ircHost config) (ircPort config)

    -- Writer to the out channel
    writer chan message = do
        let bs = encode message
        writeChan chan $ SocketData bs
        logger $ "OUT: " <> bs

    -- Processes one line
    handleLine environment handlers' inChan = do
        SocketData l <- readChan inChan
        case decode l of
            Nothing -> logger "Parse error."
            Just message' -> do
                logger $ "IN: " <> l

                -- Run every handler on the message
                forM_ handlers' $ \h -> do
                    let state = IrcState
                            { ircEnvironment = environment
                            , ircMessage = message'
                            , ircHandler = h
                            }

                    -- Run the handler in a separate thread
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
    exponentialBackoff 30 (5 * 60) $ sandBox_ logger "numberSixWith" Nothing $
        irc logger handlers' config
