module Main where

import Control.Concurrent (forkIO)
import Control.Applicative ((<$>))
import Control.Monad (forever, when, forM_, (<=<))
import System.IO
import Network (connectTo, withSocketsDo, PortID (PortNumber))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Database.Redis.Redis (connect, localhost, defaultPort)

import Network.IRC

import NumberSix.Irc
import NumberSix.Handlers

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

main :: IO ()
main = withSocketsDo $ runIrc config handlers
  where
    config = IrcConfig
        { ircNick     = "mempty"
        , ircRealName = "mempty bot"
        , ircChannels = ["#testing"]
        , ircHost     = "wina.ugent.be"
        , ircPort     = 6666
        }
