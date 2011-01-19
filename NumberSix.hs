-- | Main module, containing the 'numberSix' function needed to launch your bot.
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix
    ( numberSix
    , numberSixWith
    ) where

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException (..), catch)
import Control.Monad (forever, forM_)
import Data.Monoid (mappend)
import Control.Concurrent.Chan.Strict (readChan, writeChan)
import Control.Concurrent.MVar (newMVar)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import System.Environment (getProgName)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Message.Encode
import NumberSix.Message.Decode
import NumberSix.Handlers
import NumberSix.Socket
import NumberSix.ExponentialBackoff

-- | Run a single IRC connection
--
irc :: [SomeHandler]  -- ^ Handlers
    -> IrcConfig      -- ^ Configuration
    -> IO ()
irc handlers' config = withConnection' $ \inChan outChan -> do

    -- Create a god container
    gods <- newMVar []

    let environment = IrcEnvironment
            { ircConfig = config
            , ircWriter = writer outChan
            , ircLogger = logger
            , ircGods   = gods
            }

    -- Initialize handlers
    forM_ handlers' $ \h ->
        let state = IrcState
                { ircEnvironment = environment
                , ircMessage = error "NumberSix: message not known yet"
                , ircHandler = h
                }
        in catch (initializeSomeHandler h state)
                 (\e -> putStrLn (show (e :: SomeException)))

    forever $ handleLine environment inChan
  where
    withConnection' =
        withConnection (SBC.unpack $ ircHost config) (ircPort config)

    logger message = do
        stamp <- SBC.pack . formatTime defaultTimeLocale "%c" <$> getCurrentTime
        logFileName <- (++ ".log") <$> getProgName
        SB.appendFile logFileName $
            stamp `mappend` " " `mappend` message `mappend` "\n"

    -- Writer to the out channel
    writer chan message = do
        let bs = encode message
        writeChan chan $ SocketData bs
        logger $ "OUT: " <> bs

    -- Processes one line
    handleLine environment inChan = do
        SocketData l <- readChan inChan
        case decode l of
            Nothing -> logger "Parse error."
            Just message' -> do
                logger $ "IN: " <> l

                -- Run every handler on the message
                forM_ handlers' $ \h -> do
                    let state = IrcState
                            { ircEnvironment = environment
                            , ircMessage = message'
                            , ircHandler = h
                            }

                    -- Run the handler in a separate thread
                    _ <- forkIO $ runSomeHandler h state
                    return ()

-- | Launch a bots and block forever. All default handlers will be activated.
--
numberSix :: IrcConfig -> IO ()
numberSix = numberSixWith handlers

-- | Launch a bot with given 'SomeHandler's and block forever
--
numberSixWith :: [SomeHandler] -> IrcConfig -> IO ()
numberSixWith handlers' config = exponentialBackoff 30 $ do
    e <- try $ irc handlers' config
    putStrLn $ "Error: " ++ show (e :: Either SomeException ())
