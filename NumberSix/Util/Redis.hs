-- | Utility functions for persistence
--
module NumberSix.Util.Redis
    ( withRedis
    , getItem
    , existsItem
    , setItem
    , deleteItem
    ) where

import Data.List (intercalate)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Lazy as LB
import Data.Binary (Binary, encode, decode)
import Database.Redis.Redis

import NumberSix.Irc

-- | Construct a fully qualified key, given a simple key. This key ensures that
-- it will be unique for different hosts and channels.
--
getKey :: String             -- ^ Simple key
       -> Irc LB.ByteString  -- ^ Fully qualified key
getKey key = do
    host <- getHost
    channel <- getChannel
    handler <- getHandlerName
    return $ encode $ intercalate "-" [ "number-six", host
                                      , channel, handler, key
                                      ]

-- | Run a function with a redis connection
--
withRedis :: (Redis -> Irc a)  -- ^ Function to run with redis support
          -> Irc a             -- ^ Result
withRedis irc = do
    redis <- liftIO $ connect localhost defaultPort
    x <- irc redis
    liftIO $ disconnect redis
    return x

getItem :: Binary a => Redis -> String -> Irc (Maybe a)
getItem redis key = do
    key' <- getKey key
    reply <- liftIO $ get redis key'
    return $ case reply of RBulk (Just r) -> Just $ decode r
                           _              -> Nothing

existsItem :: Redis -> String -> Irc Bool
existsItem redis key = do
    key' <- getKey key
    reply <- liftIO $ exists redis key'
    return $ case reply of RInt 1 -> True
                           _      -> False

setItem :: Binary a => Redis -> String -> a -> Irc ()
setItem redis key item = do
    key' <- getKey key
    _ <- liftIO $ set redis key' (encode item)
    return ()

deleteItem :: Redis -> String -> Irc ()
deleteItem redis key = do
    key' <- getKey key
    _ <- liftIO $ del redis key'
    return ()
