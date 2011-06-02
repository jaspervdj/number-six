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
import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message

-- | Different realms to store values in
--
data Realm = HostRealm
           | ChannelRealm
           deriving (Show, Eq)

-- | Construct a fully qualified key, given a simple key. This key ensures that
-- it will be unique for different hosts and channels.
--
getKey :: Realm       -- ^ Realm
       -> ByteString  -- ^ Simple key
       -> Irc Key     -- ^ Fully qualified key
getKey realm key = do
    host <- getHost
    channel <- getChannel
    handler <- getHandlerName
    return $ Key $ case realm of
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
withRedis :: (Redis -> Irc a)  -- ^ Function to run with redis support
          -> Irc a             -- ^ Result
withRedis irc = do
    redis <- liftIO $ connect localhost defaultPort
    x <- irc redis
    liftIO $ disconnect redis
    return x

getItem :: Binary a => Redis -> Realm -> ByteString -> Irc (Maybe a)
getItem redis realm = liftIO . itemGet redis <=< getKey realm

existsItem :: Redis -> Realm -> ByteString -> Irc Bool
existsItem redis realm = liftIO . itemExists redis <=< getKey realm

setItem :: Binary a => Redis -> Realm -> ByteString -> a -> Irc ()
setItem redis realm key item = do
    key' <- getKey realm key
    liftIO $ itemSet redis key' item

deleteItem :: Redis -> Realm -> ByteString -> Irc ()
deleteItem redis realm = liftIO . itemDelete redis <=< getKey realm
