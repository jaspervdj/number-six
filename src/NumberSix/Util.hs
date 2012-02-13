-- | Utility functions
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util
    ( sleep
    , forkIrc
    , (<>)
    , (==?)
    , toLower
    , breakWord
    , prettyList
    , trim
    , meAction
    , kick
    , removeNewlines
    , randomElement
    , parseJsonEither
    ) where

import Control.Arrow (second)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import Data.Char (isSpace)
import System.Random (randomRIO)

import Data.Aeson (FromJSON, json, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec (parseOnly)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Message

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
    _<- liftIO . forkIO . runIrc irc =<< ask
    return ()

-- | Take a word from a string, returing the word and the remainder.
--
breakWord :: ByteString -> (ByteString, ByteString)
breakWord = second (B.drop 1) . B.break isSpace

-- | Show a list of strings in a pretty format
--
prettyList :: [ByteString] -> ByteString
prettyList [] = "none"
prettyList (x : []) = x
prettyList (x : y : []) = x <> " and " <> y
prettyList (x : y : z : r) = x <> ", " <> prettyList (y : z : r)

-- | Drop spaces around a string
--
trim :: ByteString -> ByteString
trim = B.dropWhile isSpace . B.reverse . B.dropWhile isSpace . B.reverse

-- | Make an action a /me command
--
meAction :: ByteString -> ByteString
meAction x = "\SOHACTION " <> x <> "\SOH"

-- | Kick someone
--
kick :: ByteString  -- ^ Nick to kick
     -> ByteString  -- ^ Reason
     -> Irc ()
kick nick reason = do
    channel <- getChannel
    myNick <- getNick
    -- The bot won't kick itself
    when (not $ nick ==? myNick) $
        writeMessage "KICK" [channel, nick, reason]

-- | Replace newlines by spaces
--
removeNewlines :: ByteString -> ByteString
removeNewlines = B.map (\x -> if x `elem` "\r\n" then ' ' else x)

-- | Random element from a list
--
randomElement :: [a] -> Irc a
randomElement ls = fmap (ls !!) $ liftIO $ randomRIO (0, length ls - 1)

-- | Parse JSON from a bytestring
--
parseJsonEither :: FromJSON a => ByteString -> Either String a
parseJsonEither bs = parseOnly json bs >>= parseEither parseJSON
