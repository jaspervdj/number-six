-- | Various IRC utilities
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Irc
    ( mode
    , meAction
    , kick
    ) where


--------------------------------------------------------------------------------
import           Control.Monad   (when)
import           Data.ByteString (ByteString)


--------------------------------------------------------------------------------
import           NumberSix.Irc
import           NumberSix.Util


--------------------------------------------------------------------------------
-- | Make an action a /me command
meAction :: ByteString -> ByteString
meAction x = "\SOHACTION " <> x <> "\SOH"


--------------------------------------------------------------------------------
-- | Kick someone
kick :: ByteString -> ByteString -> Irc ()
kick nick reason = do
    channel <- getChannel
    myNick <- getNick
    -- The bot won't kick itself
    when (not $ nick ==? myNick) $
        writeMessage "KICK" [channel, nick, reason]


--------------------------------------------------------------------------------
-- | Change the mode for a user
mode :: ByteString  -- ^ Mode change string (e.g. @+v@)
     -> ByteString  -- ^ Target user
     -> Irc ()      -- ^ No result
mode mode' nick = do
    channel <- getChannel
    writeMessage "MODE" [channel, mode', nick]
