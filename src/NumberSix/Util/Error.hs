--------------------------------------------------------------------------------
-- Provides a collection of appropriate error messages
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Error
    ( errors
    , randomError
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString (ByteString)


--------------------------------------------------------------------------------
import           NumberSix.Util


--------------------------------------------------------------------------------
errors :: [ByteString]
errors =
    [ "Error: please restart your computer?"
    , "I'm dealing with a terrorist threat, please hold on."
    , "Error: fire in datacenter"
    , "Socket fault, initializing postcard fallback"
    , "Error: datacenter under alien attack"
    , "Error: please insert more coins"
    , "Error: please don't do that, it really hurts :-("
    , "Error: error occurred while printing error message"
    , "OHHHH... I give up Core dumped"
    , "COMPILER UNABLE TO ABORT RUN FAST AND DON'T LOOK BACK"
    , "This is exactly the 1000th error, contact Javache for your price!"
    , "Maybe you should try asking a human?"
    , "Your request was bad, and you should feel bad"
    , "can’t go mucking with a 'void *'"
    , "I wondered how long it would take you to mess up this badly..."
    , "Invalid command. Feel ashamed for yourself and try again."
    , "Not tonight, I've got a headache"
    , "This command is available. BUT NOT FOR YOU."
    , "Error: too many regrets"
    , "Oh non! Il y a eu une erreur!"
    ]


--------------------------------------------------------------------------------
randomError :: IO ByteString
randomError = randomElement errors
