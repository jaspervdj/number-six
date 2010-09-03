-- | Utility functions
--
module NumberSix.Util
    ( sleep
    , breakWord
    , prettyTime
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
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

-- | Take a word from a string, returing the word and the remainder.
--
breakWord :: String -> (String, String)
breakWord = second (drop 1) . break isSpace

-- | Get the time in a pretty format
--
prettyTime :: Irc String
prettyTime = formatTime defaultTimeLocale "%F@%H:%M" <$> liftIO getCurrentTime
