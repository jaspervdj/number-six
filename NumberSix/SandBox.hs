-- | Provides a sandbox in which plugins can run
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.SandBox
    ( sandBox
    ) where

import Data.Monoid (mappend)
import Control.Exception (try, SomeException)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Logger

-- | Action finish signal
--
data Signal = Finished
            | Crashed ByteString
            | Timeout
            deriving (Show, Eq)

-- | Execute an IO action in a "sandbox" environment
--
sandBox :: Logger      -- ^ Logger
        -> ByteString  -- ^ Name
        -> Maybe Int   -- ^ Timeout (in seconds)
        -> (IO ())     -- ^ Sandbox action
        -> IO ()       -- ^ Blocks until timeout (or action finished)
sandBox logger name timeout action = do
    -- Communication variable
    mvar <- newEmptyMVar

    -- Thread running the action
    actionThreadId <- forkIO $ do
        r <- try action
        putMVar mvar $ case (r :: Either SomeException ()) of
            Left  e -> Crashed (SBC.pack $ show e)
            Right _ -> Finished

    -- Thread signaling a timeout (if there is a timeout)
    timeoutThreadId <- case timeout of
        Nothing -> return Nothing
        Just t  -> fmap Just $ forkIO $ do
            threadDelay (t * 1000000)
            putMVar mvar Timeout

    -- Check the result. Kill the other thread.
    result <- readMVar mvar
    case result of
        -- Timeout: kill the thread running the action
        Timeout -> killThread actionThreadId
        -- Finished/Crashed: kill the timeout thread (if it exists)
        _       -> case timeoutThreadId of
            Just id' -> killThread id'
            Nothing  -> return ()

    -- Log crashes and timeouts
    case result of
        Crashed e -> logger $
            "Thread " `mappend` name `mappend` " crashed: " `mappend` e
        Timeout   -> logger $
            "Thread " `mappend` name `mappend` " timed out"
        _         -> return ()
