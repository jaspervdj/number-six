{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
        ExistentialQuantification #-}
module NumberSix.Irc
    ( -- * Core types
      IrcConfig (..)
    , God (..)
    , IrcEnvironment (..)
    , IrcState (..)
    , Irc (..)
    , UninitializedHandler (..)
    , Handler (..)

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
    , getPrefix
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
    , writeNick

      -- * Simple responses
    , write
    , writeTo
    , writeReply

      -- * Handlers
    , makeHandler
    , makeHandlerWith
    , runHandler
    , initializeHandler

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
import NumberSix.Message.Encode (encodePrefix)

-- | User-specified IRC configuration
--
data IrcConfig = IrcConfig
    { ircNick          :: ByteString
    , ircRealName      :: ByteString
    , ircChannels      :: [ByteString]
    , ircHost          :: ByteString
    , ircPort          :: Int
    , ircGodPassword   :: ByteString
    , ircDatabase      :: String
    , ircChannelLogDir :: String
    , -- (NickServ service name, auth line)
      ircNickServ      :: Maybe (ByteString, ByteString)
    }

-- | An IRC God
--
newtype God = God {unGod :: Prefix}
            deriving (Eq)

instance Show God where
    show = SBC.unpack . encodePrefix . unGod

-- | Represents the outer IRC state
--
data IrcEnvironment = IrcEnvironment
    { ircConfig   :: IrcConfig
    , ircWriter   :: Message -> IO ()
    , ircLogger   :: ByteString -> IO ()
    , ircGods     :: MVar [God]
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
newtype Irc a = Irc {unIrc :: ReaderT IrcState IO a}
              deriving ( Monad, Functor, MonadIO
                       , MonadReader IrcState
                       )

data UninitializedHandler = forall a.
        UninitializedHandler ByteString [a -> Irc ()] (Irc a)

instance Eq UninitializedHandler where
    (UninitializedHandler x _ _) == (UninitializedHandler y _ _) = x == y

-- | Handler for IRC messages
--
data Handler = Handler
    { handlerName  :: ByteString
    , handlerHooks :: [Irc ()]
    }

-- | Run an 'Irc' action
--
runIrc :: Irc a -> IrcState -> IO a
runIrc irc state = runReaderT (unIrc irc) state

-- | Get our own nick
--
getNick :: Irc ByteString
getNick = ircNick . ircConfig . ircEnvironment <$> ask

-- | Get our real name
--
getRealName :: Irc ByteString
getRealName = ircRealName . ircConfig . ircEnvironment <$> ask

-- | Get the host we are connected to
--
getHost :: Irc ByteString
getHost = ircHost . ircConfig . ircEnvironment <$> ask

-- | Get the channels we are supposed to join
--
getChannels :: Irc [ByteString]
getChannels = ircChannels . ircConfig . ircEnvironment <$> ask

-- | Get the god password
--
getGodPassword :: Irc ByteString
getGodPassword =
    ircGodPassword . ircConfig . ircEnvironment <$> ask

-- | Get the gods of the server
--
getGods :: Irc [God]
getGods = do
    mvar <- ircGods . ircEnvironment <$> ask
    liftIO $ readMVar mvar

-- | Get the name of the current handler
--
getHandlerName :: Irc ByteString
getHandlerName = handlerName . ircHandler <$> ask

-- | Get the IRC prefix
--
getPrefix :: Irc Prefix
getPrefix = do
    Just prefix <- messagePrefix . ircMessage <$> ask
    return prefix

-- | Obtain the actual IRC command: the result from this function will always be
-- in lowercase.
--
getCommand :: Irc ByteString
getCommand = SBC.map toUpper . messageCommand . ircMessage <$> ask

-- | Obtain the IRC parameters given
--
getParameters :: Irc [ByteString]
getParameters = messageParameters . ircMessage <$> ask

-- | Obtain the sender of the command to which this handler is reacting
--
getSender :: Irc ByteString
getSender = do
    prefix <- messagePrefix . ircMessage <$> ask
    return $ case prefix of
        Nothing -> error "No sender"
        Just (ServerPrefix n) -> n
        Just (NickPrefix n _ _) -> n

-- | Get the active channel
--
getChannel :: Irc ByteString
getChannel = do
    (channel : _) <- getParameters
    return channel

-- | Obtain the message text of the command to which this handler is reacting
--
getMessageText :: Irc ByteString
getMessageText = do
    params <- getParameters
    return $ case params of
        (_ : t : _) -> t
        _ -> error "No message text"

-- | Report some message -- it will be logged
--
report :: ByteString  -- ^ Message to log
       -> Irc ()      -- ^ Result
report message = do
    logger <- ircLogger . ircEnvironment <$> ask
    liftIO $ logger $ "REPORTED: " <> message

-- | Write a raw message to the IRC socket
--
writeMessage :: ByteString    -- ^ IRC command
             -> [ByteString]  -- ^ Parameters
             -> Irc ()        -- ^ Result
writeMessage command parameters = do
    writer <- ircWriter . ircEnvironment <$> ask
    liftIO $ writer $ makeMessage command parameters

-- | Write a message in a specific channel
--
writeChannel :: ByteString  -- ^ Channel to write in
             -> ByteString  -- ^ Message text
             -> Irc ()      -- ^ Result
writeChannel destination string =
    writeMessage "PRIVMSG" [destination, string']
  where
    string' | SB.length string <= 400 = string
            | otherwise = SB.take 400 string <> "..."

-- | Write a message to a specific nick
--
writeNick :: ByteString  -- ^ The user's nickname
          -> ByteString  -- ^ Message text
          -> Irc ()
writeNick = writeChannel

-- | Write a message to the active channel
--
write :: ByteString  -- ^ Message text
      -> Irc ()      -- ^ Result
write string = do
    channel <- getChannel
    nick <- getNick
    -- If the channel equals our own nick, we are talking in a query, and we
    -- want to responsd privately to the sender.
    destination <- if nick ==? channel then getSender else return channel
    writeChannel destination string

-- | Write a message to the active channel, addressed to a certain user
--
-- Example:
--
-- > writeTo "jaspervdj" "Hello there"
--
-- Will make the bot say, in the active channel:
--
-- > jaspervdj: Hello there
--
writeTo :: ByteString  -- ^ Username to address
        -> ByteString  -- ^ Message text
        -> Irc ()      -- ^ Result
writeTo userName message = write $ userName <> ": " <> message

-- | Write a message to the active channel, addressed to the user who fired
-- the current handler. See 'writeChannelTo' as well.
--
writeReply :: ByteString  -- ^ Message text
           -> Irc ()      -- ^ Result
writeReply message = do
    sender <- getSender
    writeTo sender message

-- | Create a handler
--
makeHandler :: ByteString            -- ^ Handler name
            -> [Irc ()]              -- ^ Hooks
            -> UninitializedHandler  -- ^ Resulting handler
makeHandler name hooks = makeHandlerWith name (map const hooks) (return ())

-- | Create a handler with an initialization procedure
--
makeHandlerWith :: ByteString            -- ^ Handler name
                -> [a -> Irc ()]         -- ^ Hooks
                -> Irc a                 -- ^ Initialization
                -> UninitializedHandler  -- ^ Resulting handler
makeHandlerWith = UninitializedHandler

-- | Run a handler
--
runHandler :: Handler   -- ^ Handler to run
           -> IrcState  -- Irc state
           -> IO ()     -- ^ Result
runHandler handler state = runIrc (sequence_ $ handlerHooks handler) state

-- | Initialize a handler
--
initializeHandler :: UninitializedHandler  -- ^ Handler to initialize
                  -> IrcState              -- ^ Irc state
                  -> IO Handler            -- ^ Result
initializeHandler (UninitializedHandler name hooks ini) state = do
    x <- runIrc ini state
    return $ Handler name $ map ($ x) hooks

-- | Execute an 'Irc' action only if the command given is the command received.
--
onCommand :: ByteString  -- ^ Command to check for
          -> Irc ()      -- ^ Irc action to execute if match
          -> Irc ()      -- ^ Result
onCommand command irc = do
    actualCommand <- getCommand
    when (actualCommand ==? command) irc

-- | Execute an 'Irc' action only if the sender is a god
--
onGod :: Irc ()  -- ^ Irc action to execute if the sender is a god
      -> Irc ()  -- ^ Result
onGod irc = do
    gods <- getGods
    prefix <- getPrefix
    if God prefix `elem` gods
        then irc
        else writeReply "I laugh at your mortality."

-- | Change the list of gods
--
modifyGods :: ([God] -> [God])  -- ^ Modification
           -> ByteString        -- ^ Password
           -> Irc ()
modifyGods f password = do
    password' <- getGodPassword
    mvar <- ircGods . ircEnvironment <$> ask
    when (password == password') $ liftIO $ modifyMVar_ mvar $ return . f
