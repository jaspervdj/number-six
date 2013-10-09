-- | Utility functions
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util
    ( sleep
    , forkIrc
    , (<>)
    , (==?)
    , toLower
    , breakWord
    , prettyList
    , removeNewlines
    , randomElement
    , parseJsonEither
    , readText
    , maxLineLength
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow        (second)
import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON, json, parseJSON)
import           Data.Aeson.Types     (parseEither)
import           Data.Attoparsec      (parseOnly)
import           Data.ByteString      (ByteString)
import           Data.Char            (isSpace)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           System.Random        (randomRIO)


--------------------------------------------------------------------------------
import           NumberSix.Irc
import           NumberSix.Message


--------------------------------------------------------------------------------
-- | Sleep a while.
sleep :: Double  -- ^ Number of seconds to sleep
      -> Irc ()  -- ^ Result
sleep x = liftIO $ threadDelay (round $ x * 1000000)


--------------------------------------------------------------------------------
-- | 'forkIO' lifted to the Irc monad
forkIrc :: Irc ()  -- ^ Action to execute in another thread
        -> Irc ()  -- ^ Returns immediately
forkIrc irc = do
    _<- liftIO . forkIO . runIrc irc =<< ask
    return ()


--------------------------------------------------------------------------------
-- | Take a word from a string, returing the word and the remainder.
breakWord :: Text -> (Text, Text)
breakWord = second (T.drop 1) . T.break isSpace


--------------------------------------------------------------------------------
-- | Show a list of strings in a pretty format
prettyList :: [Text] -> Text
prettyList [] = "none"
prettyList (x : []) = x
prettyList (x : y : []) = x <> " and " <> y
prettyList (x : y : z : r) = x <> ", " <> prettyList (y : z : r)


--------------------------------------------------------------------------------
-- | Replace newlines by spaces
removeNewlines :: Text -> Text
removeNewlines = T.map (\x -> if x `elem` "\r\n" then ' ' else x)


--------------------------------------------------------------------------------
-- | Random element from a list
randomElement :: [a] -> IO a
randomElement ls = fmap (ls !!) $ randomRIO (0, length ls - 1)


--------------------------------------------------------------------------------
-- | Parse JSON from a bytestring
parseJsonEither :: FromJSON a => ByteString -> Either String a
parseJsonEither bs = parseOnly json bs >>= parseEither parseJSON


--------------------------------------------------------------------------------
-- | Read applied to a bytestring, lifted to maybe
readText :: Read a => Text -> Maybe a
readText t = case reads (T.unpack t) of
    [(x, "")] -> Just x
    _         -> Nothing


--------------------------------------------------------------------------------
-- | To prevent flooding
maxLineLength :: Int
maxLineLength = 450
