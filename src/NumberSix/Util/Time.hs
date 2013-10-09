-- | Working with time for an IRC bot is slightly confusing because it might
-- join multiple channels in different time zones. This is why we always try to
-- format times as: X hours ago, X minutes ago...
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module NumberSix.Util.Time
    ( IrcTime (..)
    , getTime
    , prettyTime
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (diffUTCTime)
import           Data.Time.Clock     (getCurrentTime)


--------------------------------------------------------------------------------
-- Basically we delegate everything to the show and read instance of UTCTime
newtype IrcTime = IrcTime {unIrcTime :: String} deriving (Show, Read)


--------------------------------------------------------------------------------
-- | Get the current time
getTime :: IO IrcTime
getTime = IrcTime . show <$> liftIO getCurrentTime


--------------------------------------------------------------------------------
-- | Get the time in a pretty format
prettyTime :: IrcTime -> IO Text
prettyTime (IrcTime time) = do
    (IrcTime now) <- getTime
    let d = floor $ toRational $ diffUTCTime (read now) (read time)
    return $ T.pack $ format d
  where
    format :: Integer -> String
    format d
        | d < minute = "less than a minute ago"
        | d < hour   = pluralize (d `div` minute) "minute"
        | d < day    = pluralize (d `div` hour)   "hour"
        | otherwise  = pluralize (d `div` day)    "day"

    minute = 60
    hour   = 60 * minute
    day    = 24 * hour

    pluralize 1 x = "1"    ++ " " ++ x ++        " ago"
    pluralize n x = show n ++ " " ++ x ++ "s" ++ " ago"
