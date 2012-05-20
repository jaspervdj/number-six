-- | Provides functions to deal with bang commands
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Bang
    ( -- * Obtaining parameters
      getBangCommand
    , getBangCommandText

      -- * Handlers
    , makeBangHandler

      -- * Reacting on events
    , onCommand
    , onBangCommand
    , onBangCommands
    ) where

import Control.Monad (when, forM_)
import Data.Char (isSpace)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Util

-- | Get the bang commmand -- a user given command, for example, @!google@.
--
getBangCommand :: Irc ByteString
getBangCommand = flip fmap getMessageText $ B.takeWhile (not .  isSpace)

-- | Get the text given to the bang command.
--
getBangCommandText :: Irc ByteString
getBangCommandText = flip fmap getMessageText $
    trim . B.dropWhile (not . isSpace)

-- | Create a simple handler with one bang hook. You should provide this
-- function with a function that produces a string to be written into the
-- channel, based on the bang command text.
--
makeBangHandler :: ByteString                      -- ^ Handler name
                -> [ByteString]                    -- ^ Bang commands
                -> (ByteString -> Irc ByteString)  -- ^ Function
                -> UninitializedHandler            -- ^ Resulting handler
makeBangHandler name commands f = makeHandler name $ return $
    onBangCommands commands $ getBangCommandText >>= f >>= write

-- | Execute an 'Irc' action only if the given bang command is executed.
--
onBangCommand :: ByteString  -- ^ Bang command (e.g. @!google@)
              -> Irc ()      -- ^ Irc action to execute if match
              -> Irc ()      -- ^ Result
onBangCommand command = onBangCommands [command]

-- | Execute an 'Irc' action only if one of the given bang commands is executed.
--
onBangCommands :: [ByteString]  -- ^ Bang commands (e.g. @!google@)
               -> Irc ()        -- ^ Irc action to execute if match
               -> Irc ()        -- ^ Result
onBangCommands commands irc = onCommand "PRIVMSG" $ do
    actualCommand <- getBangCommand
    forM_ commands $ \c -> when (actualCommand ==? c) irc
