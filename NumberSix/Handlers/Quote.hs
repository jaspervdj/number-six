{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Quote
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)
import System.Random (randomRIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Sql

handler :: Handler ByteString
handler =
    makeHandlerWith "quote" [addQuoteHook, quoteHook, lastQuoteHook] initialize

initialize :: Irc ByteString ()
initialize = withSqlRun
    "CREATE TABLE quotes (                   \
    \    id INTEGER PRIMARY KEY,             \
    \    host TEXT, channel TEXT, text TEXT  \
    \)"

addQuoteHook :: Irc ByteString ()
addQuoteHook = onBangCommand "!addquote" $ do
    text <- getBangCommandText
    host <- getHost
    channel <- getChannel
    _ <- withSql $ \c -> run c
        "INSERT INTO quotes (host, channel, text) VALUES (?, ?, ?)"
        [toSql host, toSql channel, toSql text]
    write "Quote added"

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
                qs <- filter ((query `SBC.isInfixOf`) . snd) <$> getAllQuotes
                r <- liftIO $ randomRIO (1, length qs)
                showQuote $ fst $ qs !! (r - 1)
  where
    getAllQuotes = do
        host <- getHost
        channel <- getChannel
        ls <- withSql $ \c -> quickQuery' c
            "SELECT id, text FROM quotes WHERE host = ? AND channel = ?"
            [toSql host, toSql channel]
        return $ map (\[i, t] -> (fromSql i, fromSql t)) ls

lastQuoteHook :: Irc ByteString ()
lastQuoteHook = onBangCommand "!lastquote" $ getLastId >>= showQuote

getLastId :: Irc ByteString Integer
getLastId = do
    [[r]] <- withSql $ \c -> quickQuery' c
        "SELECT MAX(id) FROM quotes" []
    return $ fromSql r

showQuote :: Integer -> Irc ByteString ()
showQuote n = do
    host <- getHost
    channel <- getChannel
    [[r]] <- withSql $ \c -> quickQuery' c
        "SELECT text FROM quotes WHERE host = ? AND channel = ? AND id = ?"
        [toSql host, toSql channel, toSql n]
    write $ "Quote " <> (SBC.pack $ show n) <> ": " <> fromSql r
