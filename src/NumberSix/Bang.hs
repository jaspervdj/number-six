--------------------------------------------------------------------------------
-- | Provides functions to deal with bang commands
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


--------------------------------------------------------------------------------
import           Control.Monad  (forM_, when)
import           Data.Char      (isSpace)
import           Data.Text      (Text)
import qualified Data.Text      as T


--------------------------------------------------------------------------------
import           NumberSix.Irc
import           NumberSix.Util


--------------------------------------------------------------------------------
-- | Get the bang commmand -- a user given command, for example, @!google@.
getBangCommand :: Irc Text
getBangCommand = flip fmap getMessageText $ T.takeWhile (not .  isSpace)


--------------------------------------------------------------------------------
-- | Get the text given to the bang command.
getBangCommandText :: Irc Text
getBangCommandText = flip fmap getMessageText $
    T.strip . T.dropWhile (not . isSpace)


--------------------------------------------------------------------------------
-- | Create a simple handler with one bang hook. You should provide this
-- function with a function that produces a string to be written into the
-- channel, based on the bang command text.
makeBangHandler :: Text                -- ^ Handler name
                -> [Text]              -- ^ Bang commands
                -> (Text -> Irc Text)  -- ^ Function
                -> UninitializedHandler            -- ^ Resulting handler
makeBangHandler name commands f = makeHandler name $ return $
    onBangCommands commands $ getBangCommandText >>= f >>= write


--------------------------------------------------------------------------------
-- | Execute an 'Irc' action only if the given bang command is executed.
onBangCommand :: Text    -- ^ Bang command (e.g. @!google@)
              -> Irc ()  -- ^ Irc action to execute if match
              -> Irc ()  -- ^ Result
onBangCommand command = onBangCommands [command]


--------------------------------------------------------------------------------
-- | Execute an 'Irc' action only if one of the given bang commands is executed.
onBangCommands :: [Text]  -- ^ Bang commands (e.g. @!google@)
               -> Irc ()        -- ^ Irc action to execute if match
               -> Irc ()        -- ^ Result
onBangCommands commands irc = onCommand "PRIVMSG" $ do
    actualCommand <- getBangCommand
    forM_ commands $ \c -> when (actualCommand ==? c) irc
