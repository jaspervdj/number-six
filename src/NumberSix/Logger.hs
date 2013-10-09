--------------------------------------------------------------------------------
-- | Logger implementation
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Logger
    ( Logger
    , makeLogger
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     ((<$>))
import           Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import           Data.Text               (Text)
import qualified Data.Text.IO            as T
import           Data.Time.Clock         (getCurrentTime)
import           Data.Time.Format        (formatTime)
import qualified System.IO               as IO
import           System.Locale           (defaultTimeLocale)


--------------------------------------------------------------------------------
type Logger = Text -> IO ()


--------------------------------------------------------------------------------
-- | Create a new logger
makeLogger :: FilePath -> IO Logger
makeLogger logName = do
    h    <- IO.openFile logName IO.AppendMode
    lock <- newMVar ()
    return $ \bs -> bs `seq` do
        ()   <- takeMVar lock
        time <- formatTime defaultTimeLocale "%c " <$> getCurrentTime
        IO.hPutStr h time
        T.hPutStrLn h bs
        IO.hFlush h
        putMVar lock ()
