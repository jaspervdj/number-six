-- | Utility functions
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util
    ( sleep
    , forkIrc
    , breakWord
    , prettyList
    , trim
    , meAction
    , kick
    , removeNewlines
    , randomElement
    ) where

import Control.Arrow (first, second)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import System.Random (randomRIO)

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

-- | Kick someone
--
kick :: IrcString s
     => s         -- ^ Nick to kick
     -> s         -- ^ Reason
     -> Irc s ()
kick nick reason = do
    channel <- getChannel
    myNick <- getNick
    when (nick /= myNick) $ writeMessage "KICK" [channel, nick, reason]

-- | Replace newlines by spaces
--
removeNewlines :: IrcString s => s -> s
removeNewlines = withIrcByteString $
    SBC.map (\x -> if x `elem` "\r\n" then ' ' else x)

-- | Random element from a list
--
randomElement :: [a] -> Irc s a
randomElement ls = fmap (ls !!) $ liftIO $ randomRIO (0, length ls - 1)
