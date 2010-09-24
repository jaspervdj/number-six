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
import Control.Monad ((<=<))

import Database.Redis.Redis
import Database.Redis.Simple
import Data.Binary (Binary)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.IrcString

-- | Construct a fully qualified key, given a simple key. This key ensures that
-- it will be unique for different hosts and channels.
--
getKey :: IrcString s
       => s            -- ^ Simple key
       -> Irc s Key    -- ^ Fully qualified key
getKey key = do
    host <- getHost
    channel <- getChannel
    handler <- getHandlerName
    return $ Key $ toByteString $
        "number-six" <> "-" <> host
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
getItem redis = liftIO . itemGet redis <=< getKey

existsItem :: IrcString s => Redis -> s -> Irc s Bool
existsItem redis = liftIO . itemExists redis <=< getKey

setItem :: (IrcString s, Binary a) => Redis -> s -> a -> Irc s ()
setItem redis key item = do
    key' <- getKey key
    liftIO $ itemSet redis key' item

deleteItem :: IrcString s => Redis -> s -> Irc s ()
deleteItem redis = liftIO . itemDelete redis <=< getKey
