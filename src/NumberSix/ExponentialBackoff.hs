-- | Exponential backoff strategy
--
module NumberSix.ExponentialBackoff
    ( exponentialBackoff
    ) where

import Control.Concurrent (threadDelay)

import Data.Time.Clock (getCurrentTime, diffUTCTime)

exponentialBackoff :: Int    -- ^ Default number of seconds
                   -> Int    -- ^ Upper bound in seconds
                   -> IO ()  -- ^ Action to run
                   -> IO ()  -- ^ Blocks forever
exponentialBackoff initialDelay upper action = exponentialBackoff' initialDelay
  where
    exponentialBackoff' delay = do
        start <- getCurrentTime
        action
        stop <- getCurrentTime

        -- The time we were executing our action
        let diff = floor $ diffUTCTime stop start

            (delay', newDelay) = if diff > initialDelay
                -- Worked for some time, try again with initial delay
                then (initialDelay, initialDelay)
                -- Very short; something obviously went wrong. Check if we have
                -- reached the upper bound.
                else if delay * 2 < upper then (delay, delay * 2)
                                          else (delay, upper)

        threadDelay $ delay' * 1000 * 1000 
        exponentialBackoff' newDelay
