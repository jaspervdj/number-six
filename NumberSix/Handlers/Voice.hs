-- | Handler to give joining peers voice, and manual control for gods
--
module NumberSix.Handlers.Voice
    ( handler
    ) where

import NumberSix.Bang
import NumberSix.Irc
import NumberSix.Util.Mode

handler :: Handler String
handler = makeHandler "voice" [autoVoiceHook, voiceHook, devoiceHook]

autoVoiceHook :: Irc String ()
autoVoiceHook = onCommand "JOIN" $ do
    sender <- getSender
    mode "+v" sender

voiceHook :: Irc String ()
voiceHook = onBangCommand "!voice" $ onGod $ do
    nick <- getBangCommandText
    mode "+v" nick

devoiceHook :: Irc String ()
devoiceHook = onBangCommands ["!devoice", "!stfu"] $ onGod $ do
    nick <- getBangCommandText
    mode "-v" nick
