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

import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Util

-- | Get the bang commmand -- a user given command, for example, @!google@.
--
getBangCommand :: IrcString s => Irc s s
getBangCommand = flip fmap getMessageText $ withIrcByteString $
    SBC.takeWhile (not .  isSpace)

-- | Get the text given to the bang command.
--
getBangCommandText :: IrcString s => Irc s s
getBangCommandText = flip fmap getMessageText $ withIrcByteString $
    trim . SBC.dropWhile (not . isSpace)

-- | Create a simple handler with one bang hook. You should provide this
-- function with a function that produces a string to be written into the
-- channel, based on the bang command text.
--
makeBangHandler :: IrcString s
                => s               -- ^ Handler name
                -> [s]             -- ^ Bang commands
                -> (s -> Irc s s)  -- ^ Function
                -> Handler s       -- ^ Resulting handler
makeBangHandler name commands f = makeHandler name $ return $
    onBangCommands commands $ getBangCommandText >>= f >>= write

-- | Execute an 'Irc' action only if the given bang command is executed.
--
onBangCommand :: IrcString s
              => s            -- ^ Bang command (e.g. @!google@)
              -> Irc s ()     -- ^ Irc action to execute if match
              -> Irc s ()     -- ^ Result
onBangCommand command = onBangCommands [command]

-- | Execute an 'Irc' action only if one of the given bang commands is executed.
--
onBangCommands :: IrcString s
               => [s]          -- ^ Bang commands (e.g. @!google@)
               -> Irc s ()     -- ^ Irc action to execute if match
               -> Irc s ()     -- ^ Result
onBangCommands commands irc = onCommand "PRIVMSG" $ do
    actualCommand <- getBangCommand
    forM_ commands $ \c -> when (actualCommand ==? c) irc
