-- | Exposes the datastructure describing an IRC message
--
module NumberSix.Message
    ( Prefix (..)
    , Message (..)
    ) where

import Data.ByteString (ByteString)

data Prefix = ServerPrefix ByteString
            | NickPrefix ByteString (Maybe ByteString) (Maybe ByteString)
            deriving (Show)

data Message = Message
    { messagePrefix     :: Maybe Prefix
    , messageCommand    :: ByteString
    , messageParameters :: [ByteString]
    } deriving (Show)
