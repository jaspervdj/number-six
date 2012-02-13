-- | Working with time for an IRC bot is slightly confusing because it might
-- join multiple channels in different time zones
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module NumberSix.Util.Time
    ( IrcTime (..)
    , getTime
    , prettyTime
    ) where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Data.Time (diffUTCTime)
import Data.Time.Clock (getCurrentTime)

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc

newtype IrcTime = IrcTime {unIrcTime :: String}
                deriving (Show, Read, Binary)

-- | Get the current time
--
getTime :: Irc IrcTime
getTime = IrcTime . show <$> liftIO getCurrentTime

-- | Get the time in a pretty format
--
prettyTime :: IrcTime -> Irc ByteString
prettyTime (IrcTime time) = do
    (IrcTime now) <- getTime
    let d = floor $ toRational $ diffUTCTime (read now) (read time)
    return $ SBC.pack $ format d
  where
    format :: Integer -> String
    format d
        | d < minute = "less than a minute ago"
        | d < hour   = pluralize (d `div` minute) "minute"
        | d < day    = pluralize (d `div` hour)   "hour"
        | otherwise  = pluralize (d `div` day)    "day"

    minute = 60
    hour = 60 * minute
    day = 24 * hour

    pluralize 1 x = "1"    ++ " " ++ x ++        " ago"
    pluralize n x = show n ++ " " ++ x ++ "s" ++ " ago"
