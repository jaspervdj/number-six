module NumberSix.Irc
    ( -- * Core types
      IrcConfig (..)
    , IrcState (..)
    , Irc
    , Handler (..)

      -- * Obtaining parameters
    , getNick
    , getRealName
    , getHost
    , getChannels
    , getHandlerName
    , getCommand
    , getParameters
    , getSender
    , getChannel
    , getMessageText
    , getBangCommand
    , getBangCommandText

      -- * Debugging
    , report

      -- * Sending responses
    , write
    , writeMessage
    , writeChannel
    , writeChannelTo
    , writeChannelReply

      -- * Handlers
    , makeHandler
    , makeBangHandler
    , runHandler

      -- * Reacting on events
    , onCommand
    , onBangCommand
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace, toLower)
import System.IO (Handle, hPutStr)

import Network.IRC (Message (..), Prefix (..), privmsg, encode)

-- | User-specified IRC configuration
--
data IrcConfig = IrcConfig
    { ircNick     :: String
    , ircRealName :: String
    , ircChannels :: [String]
    , ircHost     :: String
    , ircPort     :: Int
    }

-- | Represents the internal IRC state
--
data IrcState = IrcState
    { ircConfig   :: IrcConfig
    , ircHandle   :: Handle
    , ircMessage  :: Message
    , ircHandler  :: Handler
    , ircLogger   :: String -> IO ()
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
getNick = ircNick . ircConfig <$> ask

-- | Get our real name
--
getRealName :: Irc String
getRealName = ircRealName . ircConfig <$> ask

-- | Get the host we are connected to
--
getHost :: Irc String
getHost = ircHost . ircConfig <$> ask

-- | Get the channels we are supposed to join
--
getChannels :: Irc [String]
getChannels = ircChannels . ircConfig <$> ask

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

-- | Get the bang commmand -- a user given command, for example, @!google@.
--
getBangCommand :: Irc String
getBangCommand = map toLower . head . words <$> getMessageText

-- | Get the text given to the bang command.
--
getBangCommandText :: Irc String
getBangCommandText = drop 1 . dropWhile (not . isSpace) <$> getMessageText

-- | Report some message -- it will be logged
--
report :: String  -- ^ Message to log
       -> Irc ()  -- ^ Result
report message = do
    logger <- ircLogger <$> ask
    liftIO $ logger message

-- | Write raw text to the IRC socket
--
write :: String  -- ^ Raw text to write
      -> Irc ()  -- ^ Result
write string = do
    handle <- ircHandle <$> ask
    liftIO $ hPutStr handle $ string ++ "\r\n"
    report $ "SENT: " ++ string

-- | Write a raw message to the IRC socket
--
writeMessage :: Message  -- ^ Message to write
             -> Irc ()   -- ^ Result
writeMessage = write . encode

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

-- | Create a simple handler with one bang hook. You should provide this
-- function with a function that produces a string to be written into the
-- channel, based on the bang command text.
--
makeBangHandler :: String                  -- ^ Handler name
                -> String                  -- ^ Bang command
                -> (String -> Irc String)  -- ^ Function
                -> Handler                 -- ^ Resulting handler
makeBangHandler name command f = makeHandler name $ onBangCommand command $
    getBangCommandText >>= f >>= writeChannel

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

-- | Execute an 'Irc' action only if the given bang command is executed.
--
onBangCommand :: String  -- ^ Bang command (e.g. @!google@)
              -> Irc ()  -- ^ Irc action to execute if match
              -> Irc ()  -- ^ Result
onBangCommand command irc = onCommand "privmsg" $ do
    actualCommand <- getBangCommand
    when (actualCommand == command) irc
