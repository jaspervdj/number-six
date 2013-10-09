--------------------------------------------------------------------------------
-- | Provides a sandbox in which plugins can run
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NumberSix.SandBox
    ( sandBox
    , sandBox_
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent      (forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import           Control.Exception       (SomeException, try)
import           Data.Monoid             (mappend)
import           Data.Text               (Text)
import qualified Data.Text               as T


--------------------------------------------------------------------------------
import           NumberSix.Logger


--------------------------------------------------------------------------------
-- | Action finish signal
data Signal a
    = Finished a
    | Crashed Text
    | Timeout
    deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | Execute an IO action in a "sandbox" environment
sandBox :: forall a.
           Logger        -- ^ Logger
        -> Text          -- ^ Name
        -> Maybe Int     -- ^ Timeout (in seconds)
        -> IO a          -- ^ Sandbox action
        -> IO (Maybe a)  -- ^ Blocks until timeout (or action finished)
sandBox logger name timeout action = do
    -- Communication variable
    mvar <- newEmptyMVar

    -- Thread running the action
    actionThreadId <- forkIO $ do
        r <- try action
        putMVar mvar $ case (r :: Either SomeException a) of
            Left  e -> Crashed (T.pack $ show e)
            Right x -> Finished x

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

    -- Return result
    case result of
        Finished x -> return (Just x)
        _          -> return Nothing


--------------------------------------------------------------------------------
-- | Variation of "sandBox" for when you don't care about the result.
sandBox_ :: Logger -> Text -> Maybe Int -> IO a -> IO ()
sandBox_ logger name timeout f = do
    _ <- sandBox logger name timeout f
    return ()
