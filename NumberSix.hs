-- | Main module, containing the 'numberSix' function needed to launch your bot.
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix
    ( numberSix
    , numberSixWith
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try, SomeException (..))
import Control.Monad (forever, forM_)
import Data.Monoid (mempty, mappend)
import Control.DeepSeq (deepseq)
import Network.BSD ( HostEntry (..), getProtocolNumber, getHostByName
                   , hostAddress
                   )
import Network.Socket ( Socket, SockAddr (..), SocketType (..), socket, connect
                      , sClose )
import Network.Socket.ByteString
import Control.Concurrent.Chan (Chan, readChan, newChan, writeChan)
import Control.Concurrent.MVar (newMVar)
import System.Environment (getProgName)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Message.Encode
import NumberSix.Message.Decode
import NumberSix.Handlers

-- | Run a single IRC connection
--
irc :: [SomeHandler]  -- ^ Handlers
    -> IrcConfig      -- ^ Configuration
    -> IO ()
irc handlers' config = do
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

    _ <- loop environment sock mempty

    -- Close the socket and start again!
    sClose sock
  where
    logger message = do
        -- Create a logger
        logFileName <- (++ ".log") <$> getProgName
        SB.appendFile logFileName $ message `mappend` "\n"

    -- Higher-level connect function
    connect' hostname port = do
        protocol <- getProtocolNumber "tcp"
        entry <- getHostByName hostname
        sock <- socket (hostFamily entry) Stream protocol
        connect sock $ SockAddrInet (fromIntegral port) $ hostAddress entry
        return sock

    -- Loop forever, consuming one line every loop
    loop environment sock previous = do
        threadDelay 100000
        chunk <- fmap (previous <>) $ recv sock 4096
        if (SB.null chunk)
            then logger "Connection broke, restarting"
            else consume environment sock chunk

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
    result <- try $ SB.unpack message `deepseq` return message
    case result of
        Left (SomeException _) -> return ()
        Right m -> sendAll sock $ m <> "\r\n"

-- | Launch a bots and block forever. All default handlers will be activated.
--
numberSix :: IrcConfig -> IO ()
numberSix = numberSixWith handlers

-- | Launch a bot with given 'SomeHandler's and block forever
--
numberSixWith :: [SomeHandler] -> IrcConfig -> IO ()
numberSixWith handlers' config = do
    _ <- forever $ do
        e <- try $ irc handlers' config
        putStrLn $ "Error: " ++ show (e :: Either SomeException ())
        threadDelay 10000
    return ()
