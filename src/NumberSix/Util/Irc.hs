--------------------------------------------------------------------------------
-- | Various IRC utilities
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Irc
    ( mode
    , meAction
    , kick
    ) where


--------------------------------------------------------------------------------
import           Control.Monad  (when)
import           Data.Text      (Text)


--------------------------------------------------------------------------------
import           NumberSix.Irc
import           NumberSix.Util


--------------------------------------------------------------------------------
-- | Make an action a /me command
meAction :: Text -> Text
meAction x = "\SOHACTION " <> x <> "\SOH"


--------------------------------------------------------------------------------
-- | Kick someone
kick :: Text -> Text -> Irc ()
kick nick reason = do
    channel <- getChannel
    myNick  <- getNick
    -- The bot won't kick itself
    when (not $ nick ==? myNick) $
        writeMessage "KICK" [channel, nick, reason]


--------------------------------------------------------------------------------
-- | Change the mode for a user
mode :: Text    -- ^ Mode change string (e.g. @+v@)
     -> Text    -- ^ Target user
     -> Irc ()  -- ^ No result
mode mode' nick = do
    channel <- getChannel
    writeMessage "MODE" [channel, mode', nick]
