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
import NumberSix.Bang

handler :: Handler String
handler = makeHandler "binary" [binHook, unBinHook, hexHook, unHexHook]

binHook :: Irc String ()
binHook = onBangCommand "!bin" $ do
    n <- read <$> getBangCommandText 
    writeChannelReply $ showIntAtBase 2 intToDigit (n :: Integer) ""

unBinHook :: Irc String ()
unBinHook = onBangCommand "!unbin" $ do
    s <- getBangCommandText
    writeChannelReply $ show $ foldl (\x y -> x * 2 + digitToInt y) 0 s

hexHook :: Irc String ()
hexHook = onBangCommand "!hex" $ do
    n <- read <$> getBangCommandText
    writeChannelReply $ showHex (n :: Integer) ""

unHexHook :: Irc String ()
unHexHook = onBangCommand "!unhex" $ do
    s <- getBangCommandText
    let h = if "0x" `isPrefixOf` s then s else "0x" ++ s
    writeChannelReply $ show (read h :: Integer)
