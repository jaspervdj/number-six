-- | Ask our cylon for it's gods
--
module NumberSix.Handlers.Gods
    ( handler
    ) where

import Data.List (nub)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util

handler :: Handler String
handler = makeHandler "gods" [godsHook, addGodHook, removeGodHook]

godsHook :: Irc String ()
godsHook = onBangCommand "!gods" printGods

addGodHook :: Irc String ()
addGodHook = onBangCommand "!addgod" $ do
    password <- getBangCommandText
    sender <- getSender
    modifyGods (add sender) password
    printGods
  where
    add x = nub . (x :)

removeGodHook :: Irc String ()
removeGodHook = onBangCommand "!removegod" $ onGod $ do
    password <- getGodPassword
    god <- getBangCommandText
    sender <- getSender
    let toRemove = if null god then sender else god
    modifyGods (filter (/= toRemove)) password
    printGods

printGods :: Irc String ()
printGods = do
    gods <- getGods
    write $ "My gods are " <> prettyList gods <> "."
