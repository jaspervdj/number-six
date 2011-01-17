-- | Low-level socket code for the IRC bot
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Socket
    ( withConnection
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (forever)
import Data.Monoid (mempty, mappend)
import Network.Socket ( Socket, SocketType (..), socket, connect
                      , sClose, getAddrInfo, addrFamily, addrAddress
                      , defaultProtocol
                      )
import Network.Socket.ByteString
import Control.Concurrent.Chan (Chan, readChan, newChan, writeChan)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

-- | Run a single IRC connection
--
withConnection :: String
               -> Int
               -> (Chan SB.ByteString -> Chan SB.ByteString -> IO ())
               -> IO ()
withConnection host port application = do
    -- Connect to the server
    sock <- connect'

    -- Spawn out thread to take care of the reads
    inChan <- newChan

    -- Spawn our thread to take care of the writes
    outChan <- newChan
    writerThreadId <- forkIO $ writer outChan sock

    -- Fork the actual application
    applicationThreadId <- forkIO $ application inChan outChan

    -- Run the reader in this thread
    reader inChan sock

    -- Close the socket and start again!
    killThread applicationThreadId
    killThread writerThreadId
    sClose sock
  where
    -- Higher-level connect function
    connect' = do
        addrInfo <- head <$> getAddrInfo Nothing (Just host) (Just $ show port)
        sock <- socket (addrFamily addrInfo) Stream defaultProtocol
        connect sock $ addrAddress addrInfo
        return sock

-- | Socket -> Chan
--
reader :: Chan SB.ByteString -> Socket -> IO ()
reader chan sock = loop mempty
  where
    loop previous = do
        threadDelay 1000
        chunk <- recv sock 4096
        if SB.null chunk
            then -- Socket closed
                 return ()
            else -- We got some bytes
                 consume $ mappend previous chunk

    consume chunk =
        let (line, rest) = SBC.breakSubstring "\r\n" chunk
        in if SB.null rest
                then -- We don't have a line yet
                     loop chunk
                else -- We have at least one line to consume
                     writeChan chan line >> consume (SB.drop 2 rest)

-- | Chan -> Socket
--
writer :: Chan SB.ByteString -> Socket -> IO ()
writer chan sock = forever $ do
    -- Fully evaluate the message first
    message <- sanitize <$> readChan chan
    sendAll sock $ message `mappend` "\r\n"
  where
    -- Remove everything after a newline
    sanitize = SB.takeWhile (`SB.notElem` "\r\n")
