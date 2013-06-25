--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Rr
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.MVar   (MVar, modifyMVar, modifyMVar_,
                                            newMVar)
import           Control.Monad             (forM_)
import           Control.Monad.Trans       (liftIO)
import           Data.ByteString           (ByteString)
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           System.Random             (randomRIO)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Handlers.Insult (randomInsult)
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Irc


--------------------------------------------------------------------------------
-- | Only one game per channel
type Locks = MVar (Set ByteString)


--------------------------------------------------------------------------------
rr :: Irc ()
rr = do
    sender <- getSender
    target <- getBangCommandText
    write $
        sender <> " challenges " <> target <> " for a game of russian roulette"

    hit <- liftIO $ randomRIO (0, 5)
    let series = zip (cycle [sender, target]) $
                    replicate (hit - 1) False ++ [True]

    forM_ series $ \(shooter, isHit) -> if isHit
        then do
            write $ shooter <> " takes the gun... BANG!"
            liftIO randomInsult >>= kick shooter
        else do
            write $ shooter <> " takes the gun... CLICK!"
            sleep 0.5


--------------------------------------------------------------------------------
rrHook :: Locks -> Irc ()
rrHook mvar = onBangCommand "!rr" $ do
    channel <- getChannel
    busy    <- liftIO $ modifyMVar mvar $ \s ->
        return (S.insert channel s, S.member channel s)
    if busy
        then writeReply "A game is in progress"
        else do
            rr
            liftIO $ modifyMVar_ mvar (return . S.delete channel)


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Rr" [rrHook] $ liftIO $ newMVar S.empty
