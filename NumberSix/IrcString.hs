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
    , withIrcByteString
    , withIrcString
    , (==?)
    ) where

import Data.Monoid (Monoid)
import Data.Char (chr, toLower)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Codec.Binary.UTF8.String as Utf8
import GHC.Exts (IsString)

class (Monoid a, Eq a, Ord a, IsString a) => IrcString a where
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
        then Utf8.decode word8s
        -- Otherwise, return the simple string we found
        else simpleString
  where
    -- Construct a simple string by literally taking all the character codes
    -- from the bytestring.
    word8s = SB.unpack byteString
    simpleString = map (chr . fromIntegral) word8s

-- | To convert a 'String' to a 'ByteString', we use simple UTF-8 encoding
--
stringToByteString :: String -> ByteString
stringToByteString = SB.pack . Utf8.encode

withIrcByteString :: IrcString s
                  => (ByteString -> ByteString)
                  -> s
                  -> s
withIrcByteString f = fromByteString . f . toByteString

withIrcString :: IrcString s
              => (String -> String)
              -> s
              -> s
withIrcString f = withIrcByteString $ toByteString . f . fromByteString

-- | Case-insensitive comparison
--
(==?) :: IrcString s
      => s -> s -> Bool
s1 ==? s2 =  SBC.map toLower' (toByteString s1)
          == SBC.map toLower' (toByteString s2)
  where
    -- See IRC RFC
    toLower' '['  = '{'
    toLower' ']'  = '}'
    toLower' '\\' = '|'
    toLower' '~'  = '^'
    toLower' x    = toLower x
