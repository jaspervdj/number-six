-- | Let users obtain god rights
--
module NumberSix.Handlers.AddGod
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = Handler
    { handlerName = "addgod"
    , handlerHooks = [addGodHook, removeGodHook]
    }

addGodHook :: Irc ()
addGodHook = onBangCommand "!addgod" $ do
    password <- getBangCommandText
    sender <- getSender
    modifyGods (sender :) password

removeGodHook :: Irc ()
removeGodHook = onBangCommand "!removegod" $ onGod $ do
    password <- getGodPassword
    god <- getBangCommandText
    sender <- getSender
    let toRemove = if null god then sender else god
    modifyGods (filter (/= toRemove)) password
