-- | Exponential backoff strategy
--
module NumberSix.ExponentialBackoff
    ( exponentialBackoff
    ) where

import Control.Concurrent (threadDelay)

import Data.Time.Clock (getCurrentTime, diffUTCTime)

exponentialBackoff :: Int    -- ^ Default number of seconds
                   -> IO ()  -- ^ Action to run
                   -> IO ()  -- ^ Blocks forever
exponentialBackoff initialDelay action = exponentialBackoff' initialDelay
  where
    exponentialBackoff' delay = do
        start <- getCurrentTime
        action
        stop <- getCurrentTime

        -- The time we were executing our action
        let diff = floor $ diffUTCTime stop start

            (delay', newDelay) = if diff < initialDelay
                -- Very short; something obviously went wrong
                then (delay, delay * 2)
                else (initialDelay, initialDelay)

        threadDelay $ delay' * 1000 * 1000 
        exponentialBackoff' newDelay
