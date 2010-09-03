-- | Main module, containing the 'numberSix' function needed to launch your bot.
--
module NumberSix
    ( numberSix
    ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)
import System.IO
import Network (connectTo, withSocketsDo, PortID (PortNumber))
import Control.Monad.Reader (runReaderT)
import Database.Redis.Redis (connect, localhost, defaultPort)

import Network.IRC

import NumberSix.Irc
import NumberSix.Handlers

-- | Run a single IRC connection
--
runIrc :: IrcConfig   -- ^ Configuration
       -> [Handler]   -- ^ Handlers
       -> IO ()
runIrc config handlers' = do
    -- Connect to the IRC server and the Redis host.
    handle <- connectTo (ircHost config)
                        (PortNumber $ fromIntegral $ ircPort config)
    redis <- connect localhost defaultPort

    -- Make sure we have no buffering, so we can access all lines immediately
    -- when they are sent.
    hSetBuffering handle NoBuffering

    -- Loop forever, consuming one line every loop
    forever $ hGetLine handle >>= \line -> case decode line of
        Nothing -> logger "Parse error."
        Just message -> do
            logger $ "RECEIVED: " ++ show message

            -- Run every handler on the message
            forM_ handlers' $ \h -> do
                -- Build an IRC state
                let state = IrcState
                        { ircConfig = config
                        , ircHandle = handle
                        , ircRedis = redis
                        , ircMessage = message
                        , ircHandler = h
                        , ircLogger = logger
                        }

                -- Run the handler in a separate thread
                _ <- forkIO $ runReaderT (runHandler h) state
                return ()
  where
    logger = hPutStrLn stderr

-- | Launch multiple bots and block forever
--
numberSix :: [IrcConfig] -> IO ()
numberSix configs = withSocketsDo $ do
    -- Spawn a thread for every config
    forM_ configs $ \config -> do
        _ <- forkIO $ runIrc config handlers
        return ()

    -- Wait forever
    _ <- getContents
    return ()
