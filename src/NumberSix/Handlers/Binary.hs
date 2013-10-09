--------------------------------------------------------------------------------
-- | Binary conversions
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Binary
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Data.Char           (digitToInt, intToDigit)
import           Data.List           (isPrefixOf)
import qualified Data.Text           as T
import           Numeric             (showHex, showIntAtBase)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandler "Binary" [binHook, unBinHook, hexHook, unHexHook]


--------------------------------------------------------------------------------
binHook :: Irc ()
binHook = onBangCommand "!bin" $ do
    n <- read . T.unpack <$> getBangCommandText
    writeReply $ T.pack $ showIntAtBase 2 intToDigit (n :: Integer) ""


--------------------------------------------------------------------------------
unBinHook :: Irc ()
unBinHook = onBangCommand "!unbin" $ do
    s <- getBangCommandText
    writeReply $ T.pack $ show $ T.foldl (\x y -> x * 2 + digitToInt y) 0 s


--------------------------------------------------------------------------------
hexHook :: Irc ()
hexHook = onBangCommand "!hex" $ do
    n <- read . T.unpack <$> getBangCommandText
    writeReply $ T.pack $ showHex (n :: Integer) ""


--------------------------------------------------------------------------------
unHexHook :: Irc ()
unHexHook = onBangCommand "!unhex" $ do
    s <- T.unpack <$> getBangCommandText
    let h = if "0x" `isPrefixOf` s then s else "0x" ++ s
    writeReply $ T.pack $ show (read h :: Integer)
