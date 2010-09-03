module NumberSix
    ( numberSix
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, readMVar)
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
    handle <- connectTo (ircHost config)
                        (PortNumber $ fromIntegral $ ircPort config)
    redis <- connect localhost defaultPort
    hSetBuffering handle NoBuffering
    forever $ do
        line <- hGetLine handle
        case decode line of
            Nothing -> putStrLn "Parse error."
            Just m -> do
                putStrLn $ "RECEIVED: " ++ show m
                forM_ handlers' $ \h -> do
                    let state = IrcState
                            { ircConfig = config
                            , ircHandle = handle
                            , ircRedis = redis
                            , ircMessage = m
                            , ircHandler = h
                            , ircLogger = putStrLn
                            }
                    _ <- forkIO $ runReaderT (sequence_ $ handlerHooks h) state
                    return ()

-- | Launch multiple bots and block forever
--
numberSix :: [IrcConfig] -> IO ()
numberSix configs = withSocketsDo $ do
    forM_ configs $ \config -> do
        _ <- forkIO $ runIrc config handlers
        return ()

    -- Wait forever
    mvar <- newEmptyMVar
    () <- readMVar mvar
    return ()
