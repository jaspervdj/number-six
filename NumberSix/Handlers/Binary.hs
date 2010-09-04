-- | Binary conversions
--
module NumberSix.Handlers.Binary
    ( handler
    ) where

import Control.Applicative ((<$>))
import Data.Char (intToDigit, digitToInt)
import Data.List (isPrefixOf)
import Numeric (showIntAtBase, showHex)

import NumberSix.Irc

handler :: Handler
handler = Handler
    { handlerName = "binary"
    , handlerHooks = [binHook, unBinHook, hexHook, unHexHook]
    }

binHook :: Irc ()
binHook = onBangCommand "!bin" $ do
    n <- read <$> getBangCommandText 
    writeChannelReply $ showIntAtBase 2 intToDigit (n :: Integer) ""

unBinHook :: Irc ()
unBinHook = onBangCommand "!unbin" $ do
    s <- getBangCommandText
    writeChannelReply $ show $ unBin 0 s
  where
    unBin x [] = x
    unBin x (y : ys) = unBin (x * 2 + digitToInt y) ys

hexHook :: Irc ()
hexHook = onBangCommand "!hex" $ do
    n <- read <$> getBangCommandText
    writeChannelReply $ showHex n ""

unHexHook :: Irc ()
unHexHook = onBangCommand "!unhex" $ do
    s <- getBangCommandText
    let h = if "0x" `isPrefixOf` s then s else "0x" ++ s
    writeChannelReply $ show (read h :: Integer)
