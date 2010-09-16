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

import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Bang

handler :: Handler
handler = Handler
    { handlerName = "binary"
    , handlerHooks = [binHook, unBinHook, hexHook, unHexHook]
    }

binHook :: Irc ()
binHook = onBangCommand "!bin" $ do
    n <- read . SBC.unpack <$> getBangCommandText 
    writeChannelReply $ SBC.pack $ showIntAtBase 2 intToDigit (n :: Integer) ""

unBinHook :: Irc ()
unBinHook = onBangCommand "!unbin" $ do
    s <- getBangCommandText
    writeChannelReply $ SBC.pack $
        show $ SBC.foldl (\x y -> x * 2 + digitToInt y) 0 s

hexHook :: Irc ()
hexHook = onBangCommand "!hex" $ do
    n <- read . SBC.unpack <$> getBangCommandText
    writeChannelReply $ SBC.pack $ showHex (n :: Integer) ""

unHexHook :: Irc ()
unHexHook = onBangCommand "!unhex" $ do
    s <- SBC.unpack <$> getBangCommandText
    let h = if "0x" `isPrefixOf` s then s else "0x" ++ s
    writeChannelReply $ SBC.pack $ show (read h :: Integer)
