-- | Exposes the datastructure describing an IRC message
--
module NumberSix.Message
    ( Prefix (..)
    , Message (..)
    , makeMessage
    , (<>)
    ) where

import Data.Monoid (Monoid, mappend)

import Data.ByteString (ByteString)

data Prefix = ServerPrefix ByteString
            | NickPrefix ByteString (Maybe ByteString) (Maybe ByteString)
            deriving (Show)

data Message = Message
    { messagePrefix     :: Maybe Prefix
    , messageCommand    :: ByteString
    , messageParameters :: [ByteString]
    } deriving (Show)

makeMessage :: ByteString -> [ByteString] -> Message
makeMessage = Message Nothing

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
