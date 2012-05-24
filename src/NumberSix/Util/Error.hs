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
    , "Error: please don't do that"
    , "Error: error occurred while printing error message"
    ]


--------------------------------------------------------------------------------
randomError :: IO ByteString
randomError = randomElement errors
