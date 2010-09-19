-- | Encode IRC messages back to bytestrings
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Message.Encode where

import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (Monoid, mempty)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Message

encodePrefix :: Prefix -> ByteString
encodePrefix (ServerPrefix s) = ":" <> s
encodePrefix (NickPrefix n u h) = ":" <> n
                                <> fromMaybe "" (fmap ("!" <>) u)
                                <> fromMaybe "" (fmap ("@" <>) h)

encodeCommand :: ByteString -> ByteString
encodeCommand = id

encodeParameters :: [ByteString] -> ByteString
encodeParameters [] = mempty
encodeParameters (x : [])
    | hasSpace x || SBC.null x || SBC.head x == ':' = " :" <> x
    | otherwise = " " <> x
  where
     hasSpace = isJust . SBC.find (== ' ')
encodeParameters (x : xs) = " " <> x <> encodeParameters xs

encode :: Message -> ByteString
encode (Message p c ps) =
    encodePrefix' p <> encodeCommand c <> encodeParameters ps
  where
    encodePrefix' = fromMaybe mempty . fmap ((<> " ") . encodePrefix)
