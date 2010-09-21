-- | Utility functions for persistence
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Redis
    ( withRedis
    , getItem
    , existsItem
    , setItem
    , deleteItem
    ) where

import Control.Monad.Trans (liftIO)

import Data.Binary (Binary, encode, decode)
import Database.Redis.Redis

import NumberSix.Irc
import NumberSix.Message
import NumberSix.IrcString

-- | Construct a fully qualified key, given a simple key. This key ensures that
-- it will be unique for different hosts and channels.
--
getKey :: IrcString s
       => s            -- ^ Simple key
       -> Irc s s      -- ^ Fully qualified key
getKey key = do
    host <- getHost
    channel <- getChannel
    handler <- getHandlerName
    return $ "number-six" <> "-" <> host
                          <> "-" <> channel
                          <> "-" <> handler
                          <> "-" <> key

-- | Run a function with a redis connection
--
withRedis :: (Redis -> Irc s a)  -- ^ Function to run with redis support
          -> Irc s a             -- ^ Result
withRedis irc = do
    redis <- liftIO $ connect localhost defaultPort
    x <- irc redis
    liftIO $ disconnect redis
    return x

getItem :: (IrcString s, Binary a) => Redis -> s -> Irc s (Maybe a)
getItem redis key = do
    key' <- getKey key
    reply <- liftIO $ get redis $ toByteString key'
    return $ case reply of RBulk (Just r) -> Just $ decode r
                           _              -> Nothing

existsItem :: IrcString s => Redis -> s -> Irc s Bool
existsItem redis key = do
    key' <- getKey key
    reply <- liftIO $ exists redis $ toByteString key'
    return $ case reply of RInt 1 -> True
                           _      -> False

setItem :: (IrcString s, Binary a) => Redis -> s -> a -> Irc s ()
setItem redis key item = do
    key' <- getKey key
    _ <- liftIO $ set redis (toByteString key') (encode item)
    return ()

deleteItem :: IrcString s => Redis -> s -> Irc s ()
deleteItem redis key = do
    key' <- getKey key
    _ <- liftIO $ del redis $ toByteString key'
    return ()
