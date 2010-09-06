module NumberSix.Handlers.Quote
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, catMaybes)
import System.Random (randomRIO)

import NumberSix.Irc
import NumberSix.Util.Redis

handler :: Handler
handler = Handler
    { handlerName = "quote"
    , handlerHooks = [addQuoteHook, quoteHook, lastQuoteHook]
    }

addQuoteHook :: Irc ()
addQuoteHook = onBangCommand "!addquote" $ withRedis $ \redis -> do
    lastId <- getLastId
    text <- getBangCommandText
    let nextId = lastId + 1
    setItem redis (show nextId) text
    setItem redis "last-id" nextId
    showQuote nextId

quoteHook :: Irc ()
quoteHook = onBangCommand "!quote" $ do
    query <- getBangCommandText
    if null query
        -- No query, return a random quote
        then do
            lastId <- getLastId
            r <- liftIO $ randomRIO (1, lastId)
            showQuote r
        else if all isDigit query
            -- A number was given, lookup the quote
            then showQuote (read query)
            -- A search term was given, search through quotes
            else do
                lastId <- getLastId
                quotes <- withRedis $ \redis -> catMaybes <$>
                    forM [1 .. lastId] (getQuote redis query)
                r <- liftIO $ randomRIO (1, length quotes)
                showQuote $ quotes !! (r - 1)
  where
    getQuote redis query n = getItem redis (show n) >>= \q -> return $ case q of
        Just quote -> if query `isInfixOf` quote then Just n else Nothing
        Nothing -> Nothing

lastQuoteHook :: Irc ()
lastQuoteHook = onBangCommand "!lastquote" $ getLastId >>= showQuote

getLastId :: Irc Integer
getLastId = withRedis $ \redis -> fromMaybe 0 <$> getItem redis "last-id"

showQuote :: Integer -> Irc ()
showQuote n = do
    Just quote <- withRedis $ \redis -> getItem redis $ show n
    writeChannel $ "Quote " ++ show n ++ ": " ++ quote
