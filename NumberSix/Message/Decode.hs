-- | Module to parse in IRC messages
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Message.Decode
    ( decode
    ) where

import Data.Monoid (mempty)
import Control.Applicative ((<$>), (<|>))

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC
import Data.Attoparsec ( Parser, Result (..), parse, option, manyTill
                       , endOfInput
                       )
import Data.Attoparsec.Char8 (char8, takeTill, skipWhile)

import NumberSix.Message

-- | IRC does not consider tabs as space. Hence, we have our own isSpace
-- function here to help the parser.
--
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _ = False

prefixParser :: Parser Prefix
prefixParser = do
    _ <- char8 ':'
    prefix <- takeTill $ \x -> isSpace x || x == '!' || x == '@'
    -- A dot denotes a server prefix
    if '.' `SBC.elem` prefix
        then skipWhile isSpace >> return (ServerPrefix prefix)
        else do
            user <- option Nothing $ Just <$> do
                        _ <- char8 '!'
                        takeTill $ \x -> isSpace x || x == '@'
            host <- option Nothing $ Just <$> do
                        _ <- char8 '@'
                        takeTill isSpace
            skipWhile isSpace
            return $ NickPrefix prefix user host

commandParser :: Parser ByteString
commandParser = takeTill isSpace

parameterParser :: Parser ByteString
parameterParser = do
    skipWhile isSpace
    trailing <|> middle
  where
    trailing = char8 ':' >> takeTill (\x -> x == '\r' || x == '\n')
    middle = takeTill isSpace

messageParser :: Parser Message
messageParser = do
    prefix <- option Nothing $ Just <$> prefixParser
    command <- commandParser
    parameters <- manyTill parameterParser endOfInput
    return $ Message prefix command parameters

decode :: ByteString -> Maybe Message
decode = resultToMaybe . parse messageParser
  where
    resultToMaybe (Done _ x) = Just x
    resultToMaybe (Fail _ _ _) = Nothing
    resultToMaybe (Partial f) = case f mempty of
        Done _ x -> Just x
        _ -> Nothing
