-- | Module to parse in IRC messages
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Message.Decode
    ( decode
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<|>))
import qualified Data.Attoparsec       as A
import qualified Data.Attoparsec.Char8 as AC
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Monoid           (mempty)


--------------------------------------------------------------------------------
import           NumberSix.Message


--------------------------------------------------------------------------------
-- | IRC does not consider tabs as space. Hence, we have our own isSpace
-- function here to help the parser.
isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _    = False


--------------------------------------------------------------------------------
prefixParser :: A.Parser Prefix
prefixParser = do
    _ <- AC.char8 ':'
    prefix <- AC.takeTill $ \x -> isSpace x || x == '!' || x == '@'
    -- A dot denotes a server prefix
    if '.' `BC.elem` prefix
        then AC.skipWhile isSpace >> return (ServerPrefix prefix)
        else do
            user <- A.option Nothing $ Just <$> do
                        _ <- AC.char8 '!'
                        AC.takeTill $ \x -> isSpace x || x == '@'
            host <- A.option Nothing $ Just <$> do
                        _ <- AC.char8 '@'
                        AC.takeTill isSpace
            AC.skipWhile isSpace
            return $ NickPrefix prefix user host


--------------------------------------------------------------------------------
commandParser :: A.Parser ByteString
commandParser = AC.takeTill isSpace


--------------------------------------------------------------------------------
parameterParser :: A.Parser ByteString
parameterParser = do
    AC.skipWhile isSpace
    trailing <|> middle
  where
    trailing = AC.char8 ':' >> AC.takeTill (\x -> x == '\r' || x == '\n')
    middle = AC.takeTill isSpace


--------------------------------------------------------------------------------
messageParser :: A.Parser Message
messageParser = do
    prefix     <- A.option Nothing $ Just <$> prefixParser
    command    <- commandParser
    parameters <- A.manyTill parameterParser A.endOfInput
    return $ Message prefix command parameters


--------------------------------------------------------------------------------
decode :: ByteString -> Maybe Message
decode = resultToMaybe . A.parse messageParser
  where
    resultToMaybe (A.Done _ x)   = Just x
    resultToMaybe (A.Fail _ _ _) = Nothing
    resultToMaybe (A.Partial f)  = case f mempty of
        A.Done _ x -> Just x
        _          -> Nothing
