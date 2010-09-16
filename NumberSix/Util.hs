-- | Utility functions
--
module NumberSix.Util
    ( sleep
    , forkIrc
    , breakWord
    , prettyTime
    , prettyList
    , trim
    , meAction
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import NumberSix.Irc

-- | Sleep a while.
--
sleep :: Int     -- ^ Number of seconds to sleep
      -> Irc ()  -- ^ Result
sleep = liftIO . threadDelay . (* 1000000)

-- | 'forkIO' lifted to the Irc monad
--
forkIrc :: Irc ()  -- ^ Action to execute in another thread
        -> Irc ()  -- ^ Returns immediately
forkIrc irc = do
    _<- liftIO . forkIO . runReaderT irc =<< ask
    return ()

-- | Take a word from a string, returing the word and the remainder.
--
breakWord :: ByteString -> (ByteString, ByteString)
breakWord = second (drop 1) . break isSpace

-- | Get the time in a pretty format
--
prettyTime :: Irc ByteString
prettyTime = formatTime defaultTimeLocale "%F@%H:%M" <$> liftIO getCurrentTime

-- | Show a list of strings in a pretty format
--
prettyList :: [ByteString] -> ByteString
prettyList [] = "none"
prettyList (x : []) = x
prettyList (x : y : []) = x ++ " and " ++ y
prettyList (x : y : z : r) = x ++ ", " ++ prettyList (y : z : r)

-- | Drop spaces around a string
--
trim :: ByteString -> ByteString
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Make an action a /me command
--
meAction :: ByteString -> ByteString
meAction x = "\SOHACTION " ++ x ++ "\SOH"
