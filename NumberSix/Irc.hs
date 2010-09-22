{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
    Rank2Types, ExistentialQuantification #-}
module NumberSix.Irc
    ( -- * Core types
      IrcConfig (..)
    , IrcEnvironment (..)
    , IrcState (..)
    , Irc (..)
    , Handler (..)
    , SomeHandler (..)

      -- * Running Irc actions
    , runIrc

      -- * Obtaining parameters
    , getNick
    , getRealName
    , getHost
    , getChannels
    , getGodPassword
    , getGods
    , getHandlerName
    , getCommand
    , getParameters
    , getSender
    , getChannel
    , getMessageText

      -- * Debugging
    , report

      -- * Sending responses
    , writeMessage
    , writeChannel
    , writeChannelTo
    , writeChannelReply

      -- * Handlers
    , makeHandler
    , runHandler
    , runSomeHandler

      -- * Conditional execution
    , onCommand
    , onGod

      -- * Utility
    , modifyGods
    ) where

import Control.Concurrent (MVar, readMVar, modifyMVar_)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (toUpper)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import NumberSix.Message
import NumberSix.IrcString

-- | User-specified IRC configuration
--
data IrcConfig = IrcConfig
    { ircNick        :: ByteString
    , ircRealName    :: ByteString
    , ircChannels    :: [ByteString]
    , ircHost        :: ByteString
    , ircPort        :: Int
    , ircGodPassword :: ByteString
    }

-- | Represents the outer IRC state
--
data IrcEnvironment = IrcEnvironment
    { ircConfig   :: IrcConfig
    , ircWriter   :: Message -> IO ()
    , ircLogger   :: ByteString -> IO ()
    , ircGods     :: MVar [ByteString]
    }

-- | Represents the internal IRC state
--
data IrcState = IrcState
    { ircEnvironment :: IrcEnvironment
    , ircMessage     :: Message
    , ircHandler     :: SomeHandler
    }

-- | Monad stack for the IRC bot
--
newtype Irc s a = Irc {unIrc :: ReaderT IrcState IO a}
                deriving ( Monad, Functor, MonadIO
                         , MonadReader IrcState
                         )

-- | Handler for IRC messages
--
data Handler s = Handler
    { handlerName  :: ByteString
    , handlerHooks :: [Irc s ()]
    }

-- | Wrapper type for handlers
--
data SomeHandler = forall s. SomeHandler (Handler s)

-- | Run an 'Irc' action
--
runIrc :: Irc s a -> IrcState -> IO a
runIrc irc state = runReaderT (unIrc irc) state

-- | Get our own nick
--
getNick :: IrcString s => Irc s s
getNick = fromByteString . ircNick . ircConfig . ircEnvironment <$> ask

-- | Get our real name
--
getRealName :: IrcString s => Irc s s
getRealName = fromByteString . ircRealName . ircConfig . ircEnvironment <$> ask

-- | Get the host we are connected to
--
getHost :: IrcString s => Irc s s
getHost = fromByteString . ircHost . ircConfig . ircEnvironment <$> ask

-- | Get the channels we are supposed to join
--
getChannels :: IrcString s => Irc s [s]
getChannels =
    map fromByteString . ircChannels . ircConfig . ircEnvironment <$> ask

-- | Get the god password
--
getGodPassword :: IrcString s => Irc s s
getGodPassword =
    fromByteString . ircGodPassword . ircConfig . ircEnvironment <$> ask

-- | Get the gods of the server
--
getGods :: IrcString s => Irc s [s]
getGods = do
    mvar <- ircGods . ircEnvironment <$> ask
    gods <- liftIO $ readMVar mvar
    return $ map fromByteString gods

-- | Get the name of the current handler
--
getHandlerName :: IrcString s => Irc s s
getHandlerName = do
    (SomeHandler handler) <- ircHandler <$> ask
    return $ fromByteString $ handlerName handler

-- | Obtain the actual IRC command: the result from this function will always be
-- in lowercase.
--
getCommand :: IrcString s => Irc s s
getCommand =
    fromByteString . SBC.map toUpper . messageCommand . ircMessage <$> ask

-- | Obtain the IRC parameters given
--
getParameters :: IrcString s => Irc s [s]
getParameters = map fromByteString . messageParameters . ircMessage <$> ask

-- | Obtain the sender of the command to which this handler is reacting
--
getSender :: IrcString s => Irc s s
getSender = do
    prefix <- messagePrefix . ircMessage <$> ask
    return $ fromByteString $ case prefix of
        Nothing -> error "No sender"
        Just (ServerPrefix n) -> n
        Just (NickPrefix n _ _) -> n

-- | Get the active channel
--
getChannel :: IrcString s => Irc s s
getChannel = do
    (channel : _) <- getParameters
    return channel

-- | Obtain the message text of the command to which this handler is reacting
--
getMessageText :: IrcString s => Irc s s
getMessageText = do
    params <- getParameters
    return $ case params of
        (_ : t : _) -> t
        _ -> error "No message text"

-- | Report some message -- it will be logged
--
report :: IrcString s
       => s            -- ^ Message to log
       -> Irc s ()     -- ^ Result
report message = do
    logger <- ircLogger . ircEnvironment <$> ask
    liftIO $ logger $ toByteString $ "REPORTED: " <> message

-- | Write a raw message to the IRC socket
--
writeMessage :: IrcString s
             => s            -- ^ IRC command
             -> [s]          -- ^ Parameters
             -> Irc s ()     -- ^ Result
writeMessage command parameters = do
    writer <- ircWriter . ircEnvironment <$> ask
    let m = makeMessage (toByteString command) $ map toByteString parameters
    liftIO $ writer m

-- | Write a message to the active channel
--
writeChannel :: IrcString s
             => s            -- ^ Message text
             -> Irc s ()     -- ^ Result
writeChannel string = do
    channel <- getChannel
    nick <- getNick
    -- If the channel equals our own nick, we are talking in a query, and we
    -- want to responsd privately to the sender.
    destination <- if nick == channel then getSender else return channel
    writeMessage "PRIVMSG" [destination, string']
  where
    bs = toByteString string
    string' | SB.length bs <= 400 = string
            | otherwise = fromByteString (SB.take 400 bs) <> "..."

-- | Write a message to the active channel, addressed to a certain user
--
-- Example:
--
-- > writeChannelTo "jaspervdj" "Hello there"
--
-- Will make the bot say, in the active channel:
--
-- > jaspervdj: Hello there
--
writeChannelTo :: IrcString s
               => s            -- ^ Username to address
               -> s            -- ^ Message text
               -> Irc s ()     -- ^ Result
writeChannelTo userName message = writeChannel $ userName <> ": " <> message

-- | Write a message to the active channel, addressed to the user who fired
-- the current handler. See 'writeChannelTo' as well.
--
writeChannelReply :: IrcString s
                  => s            -- ^ Message text
                  -> Irc s ()     -- ^ Result
writeChannelReply message = do
    sender <- getSender
    writeChannelTo sender message

-- | Create a handler
--
makeHandler :: IrcString s
            => s            -- ^ Handler name
            -> [Irc s ()]   -- ^ Hooks
            -> Handler s    -- ^ Resulting handler
makeHandler name hooks = Handler (toByteString name) hooks

-- | Run a handler
--
runHandler :: Handler s  -- ^ Handler to run
           -> Irc s ()   -- ^ Result
runHandler = sequence_ . handlerHooks

-- | Run some handler
--
runSomeHandler :: SomeHandler  -- ^ Handler to run
               -> IrcState     -- ^ Irc state
               -> IO ()        -- ^ Result
runSomeHandler someHandler state = do
    (SomeHandler handler) <- return someHandler
    runIrc (runHandler handler) state

-- | Execute an 'Irc' action only if the command given is the command received.
--
onCommand :: IrcString s
          => s            -- ^ Command to check for (lowercase!)
          -> Irc s ()     -- ^ Irc action to execute if match
          -> Irc s ()     -- ^ Result
onCommand command irc = do
    actualCommand <- getCommand
    when (actualCommand == command) irc

-- | Execute an 'Irc' action only if the sender is a god
--
onGod :: IrcString s
      => Irc s ()     -- ^ Irc action to execute if the sender is a god
      -> Irc s ()     -- ^ Result
onGod irc = do
    gods <- getGods
    sender <- getSender
    if sender `elem` gods
        then irc
        else writeChannelReply "I laugh at your mortality."

-- | Change the list of gods
--
modifyGods :: IrcString s
           => ([s] -> [s])  -- ^ Modification
           -> s             -- ^ Password
           -> Irc s ()
modifyGods f password = do
    password' <- getGodPassword
    mvar <- ircGods . ircEnvironment <$> ask
    when (password == password') $ liftIO $ modifyMVar_ mvar $ return . f'
  where
    f' = map toByteString . f . map fromByteString
