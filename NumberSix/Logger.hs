-- | Logger implementation
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Logger
    ( Logger
    , newLogger
    ) where

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Monoid (mappend)
import Control.Concurrent.Chan.Strict (Chan, newChan, readChan, writeChan)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import System.Environment (getProgName)
import System.IO (openFile, IOMode (..), hFlush)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Socket

type Logger = ByteString -> IO ()

-- | Create a new logger
--
newLogger :: IO Logger
newLogger = do
    logChan <- newChan
    _ <- forkIO $ logger logChan
    return $ writeChan logChan . SocketData

-- | Logger thread
--
logger :: (Chan SocketData) -> IO ()
logger chan = do
    logName <- (++ ".log") <$> getProgName
    logHandle <- openFile logName AppendMode
    forever $ do
        SocketData message <- readChan chan
        stamp <- SBC.pack . formatTime defaultTimeLocale "%c" <$> getCurrentTime
        SB.hPutStrLn logHandle $ stamp `mappend` " " `mappend` message
        hFlush logHandle
