-- | Main module, containing the 'numberSix' function needed to launch your bot.
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix
    ( numberSix
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)
import Data.Monoid (mempty)
import Control.DeepSeq (deepseq)
import System.IO
import Network (withSocketsDo)
import Network.BSD ( HostEntry (..), getProtocolNumber, getHostByName
                   , hostAddress
                   )
import Network.Socket (Socket, SockAddr (..), SocketType (..), socket, connect)
import Network.Socket.ByteString
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
irc :: IrcConfig      -- ^ Configuration
    -> [SomeHandler]  -- ^ Handlers
    -> IO ()
irc config handlers' = do
    -- Connect to the IRC server
    sock <- connect' (SBC.unpack $ ircHost config) $ ircPort config

    -- Spawn our thread to take care of the writes
    chan <- newChan
    _ <- forkIO $ writer chan sock

    -- Create a god container
    gods <- newMVar []

    let writer' m = do
            writeChan chan m
            logger $ "SENT: " <> (SBC.pack $ show m)
        environment = IrcEnvironment
            { ircConfig = config
            , ircWriter = writer'
            , ircLogger = logger
            , ircGods   = gods
            }

    loop environment sock mempty
  where
    logger = SB.hPutStrLn stderr

    -- Higher-level connect function
    connect' hostname port = do
        protocol <- getProtocolNumber "tcp"
        entry <- getHostByName hostname
        sock <- socket (hostFamily entry) Stream protocol
        connect sock $ SockAddrInet (fromIntegral port) $ hostAddress entry
        return sock

    -- Loop forever, consuming one line every loop
    loop environment sock previous = do
        chunk <- fmap (previous <>) $ recv sock 4096
        consume environment sock chunk

    -- Consume chunks and handle lines
    consume environment sock chunk = do
        let (line, rest) = SBC.breakSubstring "\r\n" chunk
        if SB.null rest
            -- Need more input
            then loop environment sock chunk
            -- Got a line
            else do handleLine environment line
                    consume environment sock $ SB.drop 2 rest

    -- Processes one line
    handleLine environment line = case decode line of
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
                _ <- forkIO $ runSomeHandler h state
                return ()

-- | A thread that writes messages from a channel
--
writer :: Chan Message -> Socket -> IO ()
writer chan sock = forever $ do
    message <- encode <$> readChan chan
    SB.unpack message `deepseq` sendAll sock $ message <> "\r\n"

-- | Launch multiple bots and block forever
--
numberSix :: [IrcConfig] -> IO ()
numberSix configs = withSocketsDo $ do
    -- Spawn a thread for every config
    forM_ configs $ \config -> do
        _ <- forkIO $ irc config handlers
        return ()

    -- Wait forever
    interact id
