{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Application
    ( Application
    , runApplication
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Concurrent.MVar   (newMVar, putMVar, takeMVar)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.Monoid               (mempty, mappend)
import           Network.Socket            (Socket)
import qualified Network.Socket            as S
import           Network.Socket.ByteString


--------------------------------------------------------------------------------
import           NumberSix.Logger
import           NumberSix.Message
import           NumberSix.Message.Decode
import           NumberSix.Message.Encode


--------------------------------------------------------------------------------
type Application = (Message -> IO ()) -> IO (Message -> IO ())


--------------------------------------------------------------------------------
-- | Run a single IRC connection
runApplication :: Logger -> String -> Int -> Application -> IO ()
runApplication logger host port application = do
    sock   <- connect'
    writer <- makeMessageWriter logger sock
    app    <- application writer
    go sock app mempty
    logger "Server cleanly closed connection."
    S.sClose sock
  where
    -- Higher-level connect function
    connect' = do
        addr <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
        S.connect sock $ S.addrAddress addr
        return sock

    go sock app state = do
        mmsg <- readMessage logger sock state
        case mmsg of
            Nothing            -> return ()
            Just (msg, state') -> do
                logger $ "DEBUG: giving msg to app"
                _ <- app msg
                logger $ "DEBUG: app returned"
                go sock app state'


--------------------------------------------------------------------------------
type ReadState = ByteString


--------------------------------------------------------------------------------
readLine :: Logger -> Socket -> ReadState -> IO (Maybe (ByteString, ReadState))
readLine logger sock chunk
    -- We don't have a line yet
    | B.null rest = receiveMore
    -- We have at least one line to consume
    | otherwise   = return $ Just (line, B.drop 2 rest)
  where
    (line, rest) = BC.breakSubstring "\r\n" chunk
    receiveMore  = do
        logger $ "DEBUG: receiving with chunk " `B.append` BC.pack (show chunk)
        more <- recv sock 4096
        logger $ "DEBUG: received more " `B.append` BC.pack (show more)
        if B.null more
            then return Nothing
            else readLine logger sock $ mappend chunk more


--------------------------------------------------------------------------------
readMessage :: Logger -> Socket -> ReadState -> IO (Maybe (Message, ReadState))
readMessage logger sock state = do
    logger $ "DEBUG: reading line"
    mline <- readLine logger sock state
    logger $ "DEBUG: got line: " `B.append` BC.pack (show mline)
    case mline of
        Nothing             -> return Nothing
        Just (line, state') -> case decode line of
            Just msg -> do
                logger $ "IN: " `B.append` line
                return $ Just (msg, state')
            Nothing  -> do
                logger $
                    "NumberSix.Socket.readMessage: Can't parse: " `B.append`
                    line
                readMessage logger sock state'


--------------------------------------------------------------------------------
makeLineWriter :: Socket -> IO (ByteString -> IO ())
makeLineWriter sock = do
    lock <- newMVar ()
    return $ \bs -> bs `seq` do
        () <- takeMVar lock
        sendAll sock $ bs `mappend` "\r\n"
        putMVar lock ()


--------------------------------------------------------------------------------
makeMessageWriter :: Logger -> Socket -> IO (Message -> IO ())
makeMessageWriter logger sock = do
    lineWriter <- makeLineWriter sock
    return $ \msg -> do
        let bs        = encode msg
            sanitized = B.take 450 $ B.takeWhile (`B.notElem` "\r\n") bs
        logger $ "OUT: " `B.append` sanitized
        lineWriter sanitized
