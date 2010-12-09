-- | Bomb other users, a fun IRC game
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Bomb
    ( handler
    ) where

import Control.Monad (when)
import Control.Applicative ((<$>))

import Data.ByteString.Char8 as SBC

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Message
import NumberSix.Util
import NumberSix.Util.Redis

handler :: Handler ByteString
handler = makeHandler "bomb" [bombHook, passHook]

bombHook :: Irc ByteString ()
bombHook = onBangCommand "!bomb" $ do
    -- No bomb action should be running
    exists <- withRedis $ \redis -> existsItem redis "bomb"
    if exists
        then writeReply "A bomb is already set."
        else do
            (target, _) <- breakWord <$> getBangCommandText
            sender <- getSender
            when (target /= sender) $ do
                withRedis $ \r -> setItem r "bomb" (target, sender)
                bomb intervals
  where
    -- Recursively counts down
    bomb [] = do
        withBomb $ \(target, _) -> do
            write $ "The bomb explodes. Pieces of " <> target
                <> " fly in all directions."
            kick target "Ka-boom"
        withRedis $ \redis -> deleteItem redis "bomb"
    bomb (x : xs) = withBomb $ \(target, _) -> do
        write $  "Bomb attached to " <> target <> ", blowing up in "
              <> SBC.pack (show $ sum $ x : xs) <> " seconds."
        sleep x
        bomb xs

    -- Seconds
    intervals = [20, 5, 5]

-- | Pass the bomb!
--
passHook :: Irc ByteString ()
passHook = onBangCommand "!pass" $ withBomb $ \(target, attacker) -> do
    sender <- getSender
    (text, _) <- breakWord <$> getBangCommandText
    let newTarget = if SBC.null text then attacker else text
    when (sender == target) $ do
        withRedis $ \r -> setItem r "bomb" (newTarget, sender)
        write $ sender <> " passes the bomb to " <> newTarget <> "!"

-- | Utility, execute a certain action with (target, attacker)
--
withBomb :: ((ByteString, ByteString) -> Irc ByteString ())
           -> Irc ByteString ()
withBomb f = do
    tupple <- withRedis $ \redis -> getItem redis "bomb"
    case tupple of Just t  -> f t
                   Nothing -> return ()
