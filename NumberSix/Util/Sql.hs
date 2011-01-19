-- | Functions for talking to an SQL database
--
{-# LANGUAGE Rank2Types #-}
module NumberSix.Util.Sql
    ( withSql
    , withSqlRun
    , module Database.HDBC
    ) where

import Control.Monad.Trans (liftIO)

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

import NumberSix.Irc

-- | Execute a statement
--
withSql :: (forall c. IConnection c => c -> IO a) -> Irc s a
withSql f = liftIO $ do
    connection <- connectSqlite3 "number-six.db"
    x <- f connection
    commit connection
    disconnect connection
    return x

-- | Execute a run statement without arguments
--
withSqlRun :: String -> Irc s ()
withSqlRun statement = withSql $ \c -> do
    _ <- run c statement []
    return ()
