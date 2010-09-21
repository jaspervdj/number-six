-- | Utility functions
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util
    ( sleep
    , forkIrc
    , breakWord
    , prettyTime
    , prettyList
    , trim
    , meAction
    , removeNewlines
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Message

-- | Sleep a while.
--
sleep :: Int       -- ^ Number of seconds to sleep
      -> Irc s ()  -- ^ Result
sleep = liftIO . threadDelay . (* 1000000)

-- | 'forkIO' lifted to the Irc monad
--
forkIrc :: Irc s ()  -- ^ Action to execute in another thread
        -> Irc s ()  -- ^ Returns immediately
forkIrc irc = do
    _<- liftIO . forkIO . runIrc irc =<< ask
    return ()

-- | Take a word from a string, returing the word and the remainder.
--
breakWord :: IrcString s => s -> (s, s)
breakWord = first fromByteString
          . second (fromByteString . SBC.drop 1)
          . SBC.break isSpace . toByteString

-- | Get the time in a pretty format
--
prettyTime :: IrcString s => Irc s s
prettyTime = fromByteString . SBC.pack . formatTime defaultTimeLocale "%F@%H:%M"
           <$> liftIO getCurrentTime

-- | Show a list of strings in a pretty format
--
prettyList :: IrcString s => [s] -> s
prettyList [] = "none"
prettyList (x : []) = x
prettyList (x : y : []) = x <> " and " <> y
prettyList (x : y : z : r) = x <> ", " <> prettyList (y : z : r)

-- | Drop spaces around a string
--
trim :: IrcString s => s -> s
trim = withIrcByteString $
    SBC.dropWhile isSpace . SBC.reverse . SBC.dropWhile isSpace . SBC.reverse

-- | Make an action a /me command
--
meAction :: IrcString s => s -> s
meAction x = "\SOHACTION " <> x <> "\SOH"

-- | Replace newlines by spaces
--
removeNewlines :: IrcString s => s -> s
removeNewlines = withIrcByteString $
    SBC.map (\x -> if x `elem` "\r\n" then ' ' else x)
