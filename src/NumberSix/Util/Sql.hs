-- | Functions for talking to an SQL database
{-# LANGUAGE Rank2Types, OverloadedStrings #-}
module NumberSix.Util.Sql
    ( withSql
    , withSqlRun
    , createTableUnlessExists
    , module Database.HDBC
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative      ((<$>))
import           Control.Monad            (unless)
import           Control.Monad.Reader     (ask)
import           Control.Monad.Trans      (liftIO)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BC
import           Database.HDBC
import           Database.HDBC.PostgreSQL (connectPostgreSQL)


--------------------------------------------------------------------------------
import           NumberSix.Irc


--------------------------------------------------------------------------------
-- | Execute a statement
withSql :: (forall c. IConnection c => c -> IO a) -> Irc a
withSql f = do
    database <- ircDatabase . ircConfig . ircEnvironment <$> ask
    liftIO $ do
        connection <- connectPostgreSQL database
        x <- f connection
        commit connection
        disconnect connection
        return x


--------------------------------------------------------------------------------
-- | Execute a run statement without arguments
withSqlRun :: ByteString -> Irc ()
withSqlRun statement = withSql $ \c -> do
    _ <- run c (BC.unpack statement) []
    return ()


--------------------------------------------------------------------------------
-- | A bit of hack to create a table if it doesn't already exist. PostgreSQL 9.1
-- and upwards have IF NOT EXISTS, but it's not in debian stable yet...
createTableUnlessExists :: ByteString -> ByteString -> Irc ()
createTableUnlessExists name statement = withSql $ \c -> do
    tables <- getTables c
    unless (any (== BC.unpack name) tables) $ do
        _ <- run c (BC.unpack statement) []
        return ()
