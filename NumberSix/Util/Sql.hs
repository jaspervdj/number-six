-- | Functions for talking to an SQL database
--
{-# LANGUAGE Rank2Types, OverloadedStrings #-}
module NumberSix.Util.Sql
    ( withSql
    , withSqlRun
    , module Database.HDBC
    ) where

import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))

import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import NumberSix.Irc
import NumberSix.IrcString

-- | Execute a statement
--
withSql :: (IrcString s)
        => (forall c. IConnection c => c -> IO a) -> Irc s a
withSql f = do
    database <- ircDatabase . ircConfig . ircEnvironment <$> ask
    liftIO $ do
        connection <- connectPostgreSQL database
        x <- f connection
        commit connection
        disconnect connection
        return x

-- | Execute a run statement without arguments
--
withSqlRun :: IrcString s => String -> Irc s ()
withSqlRun statement = withSql $ \c -> do
    _ <- run c statement []
    return ()
