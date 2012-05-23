-- | Handler to give joining peers voice, and manual control for gods
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Voice
    ( handler
    ) where


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util.Irc


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandler "voice" [autoVoiceHook, voiceHook, devoiceHook]


--------------------------------------------------------------------------------
autoVoiceHook :: Irc ()
autoVoiceHook = onCommand "JOIN" $ do
    sender <- getSender
    mode "+v" sender


--------------------------------------------------------------------------------
voiceHook :: Irc ()
voiceHook = onBangCommand "!voice" $ onGod $ do
    nick <- getBangCommandText
    mode "+v" nick


--------------------------------------------------------------------------------
devoiceHook :: Irc ()
devoiceHook = onBangCommands ["!devoice", "!stfu"] $ onGod $ do
    nick <- getBangCommandText
    mode "-v" nick
