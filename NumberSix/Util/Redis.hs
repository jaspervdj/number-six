-- | Utility functions for persistence
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Util.Redis
    ( Realm (..)
    , withRedis
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

-- | Different realms to store values in
--
data Realm = HostRealm
           | ChannelRealm
           deriving (Show, Eq)

-- | Construct a fully qualified key, given a simple key. This key ensures that
-- it will be unique for different hosts and channels.
--
getKey :: IrcString s
       => Realm        -- ^ Realm
       -> s            -- ^ Simple key
       -> Irc s Key    -- ^ Fully qualified key
getKey realm key = do
    host <- getHost
    channel <- getChannel
    handler <- getHandlerName
    return $ Key $ toByteString $ case realm of
        HostRealm    -> "number-six" <> "-" <> "host"
                                     <> "-" <> host
                                     <> "-" <> handler
                                     <> "-" <> key
        ChannelRealm -> "number-six" <> "-" <> "channel"
                                     <> "-" <> host
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

getItem :: (IrcString s, Binary a) => Redis -> Realm -> s -> Irc s (Maybe a)
getItem redis realm = liftIO . itemGet redis <=< getKey realm

existsItem :: IrcString s => Redis -> Realm -> s -> Irc s Bool
existsItem redis realm = liftIO . itemExists redis <=< getKey realm

setItem :: (IrcString s, Binary a) => Redis -> Realm -> s -> a -> Irc s ()
setItem redis realm key item = do
    key' <- getKey realm key
    liftIO $ itemSet redis key' item

deleteItem :: IrcString s => Redis -> Realm -> s -> Irc s ()
deleteItem redis realm = liftIO . itemDelete redis <=< getKey realm
