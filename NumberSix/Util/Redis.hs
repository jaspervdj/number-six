module NumberSix.Util.Redis
    ( getItem
    , existsItem
    , setItem
    , deleteItem
    ) where

import Data.List (intercalate)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Binary (Binary, encode, decode)
import Database.Redis.Redis

import NumberSix.Irc

getKey :: String             -- ^ Simple key
       -> Irc LB.ByteString  -- ^ Fully qualified key
getKey key = do
    host <- getHost
    channel <- getChannel
    handler <- getHandlerName
    return $ encode $ intercalate "-" [host, channel, handler, key]

getItem :: Binary a => String -> Irc (Maybe a)
getItem key = do
    redis <- getRedis
    key' <- getKey key
    reply <- liftIO $ get redis key'
    return $ case reply of RBulk (Just r) -> Just $ decode r
                           _              -> Nothing

existsItem :: String -> Irc Bool
existsItem key = do
    redis <- getRedis
    key' <- getKey key
    reply <- liftIO $ exists redis key'
    return $ case reply of RInt 1 -> True
                           _      -> False

setItem :: Binary a => String -> a -> Irc ()
setItem key item = do
    redis <- getRedis
    key' <- getKey key
    _ <- liftIO $ set redis key' (encode item)
    return ()

deleteItem :: String -> Irc ()
deleteItem key = do
    redis <- getRedis
    key' <- getKey key
    _ <- liftIO $ del redis key'
    return ()
