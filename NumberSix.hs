-- | Main module, containing the 'numberSix' function needed to launch your bot.
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix
    ( numberSix
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)
import Control.DeepSeq (deepseq)
import System.IO
import Network (connectTo, withSocketsDo, PortID (PortNumber))
import Control.Monad.Reader (runReaderT)
import Control.Concurrent.Chan (Chan, readChan, newChan, writeChan)
import Control.Concurrent.MVar (newMVar)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Message.Encode
import NumberSix.Message.Decode
import NumberSix.Handlers

-- | Run a single IRC connection
--
runIrc :: IrcConfig   -- ^ Configuration
       -> [Handler]   -- ^ Handlers
       -> IO ()
runIrc config handlers' = do
    -- Connect to the IRC server
    handle <- connectTo (SBC.unpack $ ircHost config)
                        (PortNumber $ fromIntegral $ ircPort config)

    -- Use UTF-8 by default
    hSetEncoding handle utf8

    -- Make sure we have no buffering, so we can access all lines immediately
    -- when they are sent.
    hSetBuffering handle NoBuffering

    -- Spawn our thread to take care of the writes
    chan <- newChan
    _ <- forkIO $ writer chan handle

    -- Create a god container
    gods <- newMVar []

    let writer m = do
            writeChan chan m
            logger $ "SENT: " <> (SBC.pack $ show m)
        environment = IrcEnvironment
            { ircConfig = config
            , ircWriter = writer
            , ircLogger = logger
            , ircGods   = gods
            }

    -- Loop forever, consuming one line every loop
    forever $ hGetLine handle >>= \line -> case decode (SBC.pack line) of
        Nothing -> logger "Parse error."
        Just message' -> do
            logger $ "RECEIVED: " <> (SBC.pack $ show message')

                -- Build an IRC state
            -- Run every handler on the message
            forM_ handlers' $ \h -> do
                let state = IrcState
                        { ircEnvironment = environment
                        , ircMessage = message'
                        , ircHandler = h
                        }

                -- Run the handler in a separate thread
                _ <- forkIO $ runReaderT (runHandler h) state
                return ()
  where
    logger = SB.hPutStrLn stderr

-- | A thread that writes messages from a channel
--
writer :: Chan Message -> Handle -> IO ()
writer chan handle = forever $ do
    message <- encode <$> readChan chan
    SB.unpack message `deepseq` SB.hPutStr handle $ message <> "\r\n"

-- | Launch multiple bots and block forever
--
numberSix :: [IrcConfig] -> IO ()
numberSix configs = withSocketsDo $ do
    -- Spawn a thread for every config
    forM_ configs $ \config -> do
        _ <- forkIO $ runIrc config handlers
        return ()

    -- Wait forever
    interact id
