-- | Encode IRC messages back to bytestrings
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Message.Encode
    ( encodePrefix
    , encode
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 ()
import           Data.Maybe            (fromMaybe, isJust)
import           Data.Monoid           (Monoid, mempty)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T


--------------------------------------------------------------------------------
import           NumberSix.Message


--------------------------------------------------------------------------------
encodePrefix :: Prefix -> ByteString
encodePrefix (ServerPrefix s)   = ":" <> T.encodeUtf8 s
encodePrefix (NickPrefix n u h) =
    ":" <> T.encodeUtf8 n <>
        fromMaybe "" (fmap (("!" <>) . T.encodeUtf8) u) <>
        fromMaybe "" (fmap (("@" <>) . T.encodeUtf8) h)


--------------------------------------------------------------------------------
encodeCommand :: Text -> ByteString
encodeCommand = T.encodeUtf8


--------------------------------------------------------------------------------
encodeParameters :: [Text] -> ByteString
encodeParameters [] = mempty
encodeParameters (x : [])
    | hasSpace x || T.null x || T.head x == ':' = " :" <> T.encodeUtf8 x
    | otherwise                                 = " "  <> T.encodeUtf8 x
  where
     hasSpace = isJust . T.find (== ' ')
encodeParameters (x : xs) = " " <> T.encodeUtf8 x <> encodeParameters xs


--------------------------------------------------------------------------------
encode :: Message -> ByteString
encode (Message p c ps) =
    encodePrefix' p <> encodeCommand c <> encodeParameters ps
  where
    encodePrefix' = fromMaybe mempty . fmap ((<> " ") . encodePrefix)
