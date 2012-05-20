-- | Binary conversions
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Binary
    ( handler
    ) where

import Control.Applicative ((<$>))
import Data.Char (intToDigit, digitToInt)
import Data.List (isPrefixOf)
import Numeric (showIntAtBase, showHex)

import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Bang

handler :: UninitializedHandler
handler = makeHandler "binary" [binHook, unBinHook, hexHook, unHexHook]

binHook :: Irc ()
binHook = onBangCommand "!bin" $ do
    n <- read . B.unpack <$> getBangCommandText 
    writeReply $ B.pack $ showIntAtBase 2 intToDigit (n :: Integer) ""

unBinHook :: Irc ()
unBinHook = onBangCommand "!unbin" $ do
    s <- getBangCommandText
    writeReply $ B.pack $ show $ B.foldl (\x y -> x * 2 + digitToInt y) 0 s

hexHook :: Irc ()
hexHook = onBangCommand "!hex" $ do
    n <- read . B.unpack <$> getBangCommandText
    writeReply $ B.pack $ showHex (n :: Integer) ""

unHexHook :: Irc ()
unHexHook = onBangCommand "!unhex" $ do
    s <- B.unpack <$> getBangCommandText
    let h = if "0x" `isPrefixOf` s then s else "0x" ++ s
    writeReply $ B.pack $ show (read h :: Integer)
