-- | Utility functions
--
module NumberSix.Util
    ( sleep
    , forkIrc
    , breakWord
    , prettyTime
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
breakWord :: String -> (String, String)
breakWord = second (drop 1) . break isSpace

-- | Get the time in a pretty format
--
prettyTime :: Irc String
prettyTime = formatTime defaultTimeLocale "%F@%H:%M" <$> liftIO getCurrentTime

-- | Drop spaces around a string
--
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Make an action a /me command
--
meAction :: String -> String
meAction x = "\SOHACTION " ++ x ++ "\SOH"
