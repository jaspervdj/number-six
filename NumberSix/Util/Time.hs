-- | Working with time for an IRC bot is slightly confusing because it might
-- join multiple channels in different time zones
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module NumberSix.Util.Time
    ( IrcTime
    , getTime
    , prettyTime
    ) where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Data.Time (diffUTCTime)
import Data.Time.Clock (getCurrentTime)

import qualified Data.ByteString.Char8 as SBC
import Data.Binary (Binary)

import NumberSix.Irc
import NumberSix.IrcString

newtype IrcTime = IrcTime String
                deriving (Show, Read, Binary)

-- | Get the current time
--
getTime :: Irc s IrcTime
getTime = IrcTime . show <$> liftIO getCurrentTime

-- | Get the time in a pretty format
--
prettyTime :: IrcString s => IrcTime -> Irc s s
prettyTime (IrcTime time) = do
    (IrcTime now) <- getTime
    let d = floor $ toRational $ diffUTCTime (read now) (read time)
    return $ fromByteString $ SBC.pack $ format d
  where
    format :: Integer -> String
    format d
        | d < minute = "less than a minute ago"
        | d < hour = show (d `div` minute) ++ " minutes ago"
        | d < day = show (d `div` hour) ++ " hours ago"
        | otherwise = show (d `div` day) ++ " days ago"

    minute = 60
    hour = 60 * minute
    day = 24 * hour
