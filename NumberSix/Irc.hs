module NumberSix.Irc
    ( -- * Core types
      IrcConfig (..)
    , IrcEnvironment (..)
    , IrcState (..)
    , Irc
    , Handler (..)

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

      -- * Conditional execution
    , onCommand
    , onGod

      -- * Utility
    , modifyGods
    ) where

import Control.Concurrent (MVar, readMVar, modifyMVar_)
import Control.Applicative ((<$>))
import Control.DeepSeq (deepseq)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import System.IO (Handle, hPutStr)

import Network.IRC (Message (..), Prefix (..), privmsg, encode)

-- | User-specified IRC configuration
--
data IrcConfig = IrcConfig
    { ircNick        :: String
    , ircRealName    :: String
    , ircChannels    :: [String]
    , ircHost        :: String
    , ircPort        :: Int
    , ircGodPassword :: String
    }

-- | Represents the outer IRC state
--
data IrcEnvironment = IrcEnvironment
    { ircConfig   :: IrcConfig
    , ircWriter   :: Message -> IO ()
    , ircLogger   :: String -> IO ()
    , ircGods     :: MVar [String]
    }

-- | Represents the internal IRC state
--
data IrcState = IrcState
    { ircEnvironment :: IrcEnvironment
    , ircMessage     :: Message
    , ircHandler     :: Handler
    }

-- | Monad stack for the IRC bot
--
type Irc = ReaderT IrcState IO

-- | Handler for IRC messages
--
data Handler = Handler
    { handlerName  :: String
    , handlerHooks :: [Irc ()]
    }

-- | Get our own nick
--
getNick :: Irc String
getNick = ircNick . ircConfig . ircEnvironment <$> ask

-- | Get our real name
--
getRealName :: Irc String
getRealName = ircRealName . ircConfig . ircEnvironment <$> ask

-- | Get the host we are connected to
--
getHost :: Irc String
getHost = ircHost . ircConfig . ircEnvironment <$> ask

-- | Get the channels we are supposed to join
--
getChannels :: Irc [String]
getChannels = ircChannels . ircConfig . ircEnvironment <$> ask

-- | Get the god password
--
getGodPassword :: Irc String
getGodPassword = ircGodPassword . ircConfig . ircEnvironment <$> ask

-- | Get the gods of the server
--
getGods :: Irc [String]
getGods = do
    mvar <- ircGods . ircEnvironment <$> ask
    liftIO $ readMVar mvar

-- | Get the name of the current handler
--
getHandlerName :: Irc String
getHandlerName = handlerName . ircHandler <$> ask

-- | Obtain the actual IRC command: the result from this function will always be
-- in lowercase.
--
getCommand :: Irc String
getCommand = map toLower . msg_command . ircMessage <$> ask

-- | Obtain the IRC parameters given
--
getParameters :: Irc [String]
getParameters = msg_params . ircMessage <$> ask

-- | Obtain the sender of the command to which this handler is reacting
--
getSender :: Irc String
getSender = do
    prefix <- msg_prefix . ircMessage <$> ask
    return $ case prefix of
        Nothing -> error "No sender"
        Just (Server n) -> n
        Just (NickName n _ _) -> n

-- | Get the active channel
--
getChannel :: Irc String
getChannel = do
    (channel : _) <- getParameters
    return channel

-- | Obtain the message text of the command to which this handler is reacting
--
getMessageText :: Irc String
getMessageText = do
    params <- getParameters
    return $ case params of
        (_ : t : _) -> t
        _ -> error "No message text"

-- | Report some message -- it will be logged
--
report :: String  -- ^ Message to log
       -> Irc ()  -- ^ Result
report message = do
    logger <- ircLogger . ircEnvironment <$> ask
    liftIO $ logger $ "REPORTED: " ++ message

-- | Write a raw message to the IRC socket
--
writeMessage :: Message  -- ^ Message to write
             -> Irc ()   -- ^ Result
writeMessage message = do
    writer <- ircWriter . ircEnvironment <$> ask
    liftIO $ writer message

-- | Write a message to the active channel
--
writeChannel :: String  -- ^ Message text
             -> Irc ()  -- ^ Result
writeChannel string = do
    channel <- getChannel
    writeMessage $ privmsg channel string

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
writeChannelTo :: String  -- ^ Username to address
               -> String  -- ^ Message text
               -> Irc ()  -- ^ Result
writeChannelTo userName message = writeChannel $ userName ++ ": " ++ message

-- | Write a message to the active channel, addressed to the user who fired
-- the current handler. See 'writeChannelTo' as well.
--
writeChannelReply :: String  -- ^ Message text
                  -> Irc ()  -- ^ Result
writeChannelReply message = do
    sender <- getSender
    writeChannelTo sender message

-- | Create a simple handler with one hook
--
makeHandler :: String   -- ^ Handler name
            -> Irc ()   -- ^ Hook
            -> Handler  -- ^ Resulting handler
makeHandler name irc = Handler name [irc]

-- | Run a handler
--
runHandler :: Handler  -- ^ Handler to run
           -> Irc ()   -- ^ Result
runHandler = sequence_ . handlerHooks

-- | Execute an 'Irc' action only if the command given is the command received.
--
onCommand :: String  -- ^ Command to check for (lowercase!)
          -> Irc ()  -- ^ Irc action to execute if match
          -> Irc ()  -- ^ Result
onCommand command irc = do
    actualCommand <- getCommand
    when (actualCommand == command) irc

-- | Execute an 'Irc' action only if the sender is a god
--
onGod :: Irc ()  -- ^ Irc action to execute if the sender is a god
      -> Irc ()  -- ^ Result
onGod irc = do
    gods <- getGods
    sender <- getSender
    if sender `elem` gods
        then irc
        else writeChannelReply "I laugh at your mortality."

-- | Change the list of gods
--
modifyGods :: ([String] -> [String])  -- ^ Modification
           -> String                  -- ^ Password
           -> Irc ()
modifyGods f password = do
    password' <- getGodPassword
    mvar <- ircGods . ircEnvironment <$> ask
    when (password == password') $ liftIO $ modifyMVar_ mvar $ return . f
