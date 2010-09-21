-- | The IRC protocol does not impose a certain string encoding to clients or
-- servers. Therefore, we don't actually know what format we will receive text
-- in.
--
-- To solve this problem, we use an 'IrcString' typeclass, which supports
-- different string formats.
--
{-# LANGUAGE FlexibleInstances #-}
module NumberSix.IrcString
    ( IrcString (..)
    ) where

import Data.Monoid (Monoid)
import Data.Char (chr, ord)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Codec.Binary.UTF8.String as Utf8

class Monoid a => IrcString a where
    fromByteString :: ByteString -> a
    toByteString :: a -> ByteString

instance IrcString ByteString where
    fromByteString = id
    toByteString = id

instance IrcString [Char] where
    fromByteString = byteStringToString
    toByteString = stringToByteString

-- | Converts a 'ByteString' to a 'String'
--
byteStringToString :: ByteString -> String
byteStringToString byteString =
    -- Check if the input is UTF-8 encoded
    if Utf8.isUTF8Encoded simpleString
        -- If so, decode
        then Utf8.decode words
        -- Otherwise, return the simple string we found
        else simpleString
  where
    -- Construct a simple string by literally taking all the character codes
    -- from the bytestring.
    words = SB.unpack byteString
    simpleString = map (chr . fromIntegral) words

-- | To convert a 'String' to a 'ByteString', we use simple UTF-8 encoding
--
stringToByteString :: String -> ByteString
stringToByteString = SB.pack . Utf8.encode
