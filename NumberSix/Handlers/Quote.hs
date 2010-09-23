{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Quote
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, catMaybes)
import System.Random (randomRIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Redis

handler :: Handler ByteString
handler = makeHandler "quote" [addQuoteHook, quoteHook, lastQuoteHook]

addQuoteHook :: Irc ByteString ()
addQuoteHook = onBangCommand "!addquote" $ withRedis $ \redis -> do
    lastId <- getLastId
    text <- getBangCommandText
    let nextId = lastId + 1
    setItem redis (SBC.pack $ show nextId) text
    setItem redis "last-id" nextId
    showQuote nextId

quoteHook :: Irc ByteString ()
quoteHook = onBangCommand "!quote" $ do
    query <- getBangCommandText
    if SBC.null query
        -- No query, return a random quote
        then do
            lastId <- getLastId
            r <- liftIO $ randomRIO (1, lastId)
            showQuote r
        else if SBC.all isDigit query
            -- A number was given, lookup the quote
            then showQuote (read $ SBC.unpack query)
            -- A search term was given, search through quotes
            else do
                lastId <- getLastId
                quotes <- withRedis $ \redis -> catMaybes <$>
                    forM [1 .. lastId] (getQuote redis query)
                r <- liftIO $ randomRIO (1, length quotes)
                showQuote $ quotes !! (r - 1)
  where
    getQuote redis query n = do
        item <- getItem redis query
        return $ case item of
            Nothing -> Nothing
            Just quote -> if query `SBC.isInfixOf` quote then Just n
                                                         else Nothing

lastQuoteHook :: Irc ByteString ()
lastQuoteHook = onBangCommand "!lastquote" $ getLastId >>= showQuote

getLastId :: Irc ByteString Integer
getLastId = withRedis $ \redis -> fromMaybe 0 <$> getItem redis "last-id"

showQuote :: Integer -> Irc ByteString ()
showQuote n = do
    let sn = SBC.pack $ show n
    Just quote <- withRedis $ \redis -> getItem redis sn
    write $ "Quote " <> sn <> ": " <> quote
