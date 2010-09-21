-- | Let users obtain god rights
--
module NumberSix.Handlers.AddGod
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang

handler :: Handler String
handler = makeHandler "addgod" [addGodHook, removeGodHook]

addGodHook :: Irc String ()
addGodHook = onBangCommand "!addgod" $ do
    password <- getBangCommandText
    sender <- getSender
    modifyGods (sender :) password

removeGodHook :: Irc String ()
removeGodHook = onBangCommand "!removegod" $ onGod $ do
    password <- getGodPassword
    god <- getBangCommandText
    sender <- getSender
    let toRemove = if null god then sender else god
    modifyGods (filter (/= toRemove)) password
