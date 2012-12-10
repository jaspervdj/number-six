-- | Bomb other users, a fun IRC game
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Bomb
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Concurrent.MVar
import           Control.Monad.Trans       (liftIO)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as SBC
import           Data.Foldable             (forM_)
import           Data.Map                  (Map)
import qualified Data.Map                  as M


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Handlers.Insult (randomInsult)
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Irc


--------------------------------------------------------------------------------
-- | A bomb is assigned through (target, sender)
type Bomb = (ByteString, ByteString)


--------------------------------------------------------------------------------
-- | Bomb state: each channel may have a bomb assigned
type BombState = Map ByteString Bomb


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Bomb" [bombHook, passHook] $ liftIO $ newMVar M.empty


--------------------------------------------------------------------------------
bombHook :: MVar BombState -> Irc ()
bombHook mvar = onBangCommand "!bomb" $ do
    updateBomb mvar $ \b -> case b of
        Just _  -> (write =<< liftIO randomError) >> return b
        Nothing -> do
            (target, _) <- breakWord <$> getBangCommandText
            sender      <- getSender
            nick        <- getNick
            if target == sender || target == nick
                then do
                    liftIO randomInsult >>= kick sender
                    return Nothing
                else do
                    forkIrc $ bomb intervals
                    return $ Just (target, sender)
  where
    -- Recursively counts down
    bomb [] = updateBomb mvar $ \b -> do
        forM_ b $ \(target, _) -> do
            write $ "The bomb explodes. Pieces of " <> target <>
                " fly in all directions."
            kick target "Ka-boom"
        return Nothing
    bomb (x : xs) = do
        updateBomb mvar $ \b -> do
            forM_ b $ \(target, _) ->
                write $ "Bomb attached to " <> target <> ", blowing up in " <>
                    SBC.pack (show $ sum $ x : xs) <> " seconds."
            return b
        sleep x
        bomb xs

    intervals = [20, 5, 5]


--------------------------------------------------------------------------------
-- | Pass the bomb!
passHook :: MVar BombState -> Irc ()
passHook mvar = onBangCommand "!pass" $ do
    sender    <- getSender
    (text, _) <- breakWord <$> getBangCommandText
    updateBomb mvar $ \bomb -> case bomb of
        Nothing                 -> return Nothing
        Just (target, attacker)
            | sender ==? target -> do
                let newTarget = if SBC.null text then attacker else text
                write $ sender <> " passes the bomb to " <> newTarget <> "!"
                return $ Just (newTarget, sender)
            -- Nothing changes
            | otherwise         -> return $ Just (target, attacker)


--------------------------------------------------------------------------------
-- | Utility, execute a certain action with (target, attacker)
updateBomb :: MVar BombState
           -> (Maybe Bomb -> Irc (Maybe Bomb))
           -> Irc ()
updateBomb mvar f = do
    m    <- liftIO $ takeMVar mvar
    chan <- getChannel
    x    <- f (M.lookup chan m)
    liftIO $ putMVar mvar $ case x of
        Just x' -> M.insert chan x' m
        Nothing -> M.delete chan m
