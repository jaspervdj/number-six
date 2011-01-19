-- | Low-level socket code for the IRC bot
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Socket
    ( SocketData (..)
    , withConnection
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forever, unless)
import Data.Monoid (mempty, mappend)
import Control.Concurrent.Chan.Strict (Chan, readChan, newChan, writeChan)

import Data.ByteString (ByteString)
import Control.DeepSeq (NFData (..))
import Network.Socket ( Socket, SocketType (..), socket, connect
                      , sClose, getAddrInfo, addrFamily, addrAddress
                      , defaultProtocol
                      )
import Network.Socket.ByteString

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

-- | Newtype for data sent to a socket
--
newtype SocketData = SocketData {unSocketData :: ByteString}
                   deriving (Show)

instance NFData SocketData where
    rnf = rnf . SB.unpack . unSocketData

-- | Run a single IRC connection
--
withConnection :: String
               -> Int
               -> (Chan SocketData -> Chan SocketData -> IO ())
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
reader :: Chan SocketData -> Socket -> IO ()
reader chan sock = loop mempty
  where
    loop previous = do
        chunk <- recv sock 4096
        unless (SB.null chunk) $ consume $ mappend previous chunk

    consume chunk =
        let (line, rest) = SBC.breakSubstring "\r\n" chunk
        in if SB.null rest
                then -- We don't have a line yet
                     loop chunk
                else -- We have at least one line to consume
                     do writeChan chan (SocketData line)
                        consume (SB.drop 2 rest)

-- | Chan -> Socket
--
writer :: Chan SocketData -> Socket -> IO ()
writer chan sock = forever $ do
    message <- sanitize . unSocketData <$> readChan chan
    sendAll sock $ message `mappend` "\r\n"
  where
    -- Remove everything after a newline
    sanitize = SB.take 450 . SB.takeWhile (`SB.notElem` "\r\n")
