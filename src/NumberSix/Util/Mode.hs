-- | Utility handlers to change channel modes
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Mode
    ( mode
    ) where

import Data.ByteString (ByteString)

import NumberSix.Irc

-- | Change the mode for a user
--
mode :: ByteString  -- ^ Mode change string (e.g. @+v@)
     -> ByteString  -- ^ Target user
     -> Irc ()      -- ^ No result
mode mode' nick = do
    channel <- getChannel
    writeMessage "MODE" [channel, mode', nick]
