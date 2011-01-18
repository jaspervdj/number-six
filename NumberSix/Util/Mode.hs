-- | Utility handlers to change channel modes
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Mode
    ( mode
    ) where

import NumberSix.Irc
import NumberSix.IrcString

-- | Change the mode for a user
--
mode :: IrcString s
     => s         -- ^ Mode change string (e.g. @+v@)
     -> s         -- ^ Target user
     -> Irc s ()  -- ^ No result
mode mode' nick = do
    channel <- getChannel
    writeMessage "MODE" [channel, mode', nick]
