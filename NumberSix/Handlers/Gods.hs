-- | Ask our cylon for it's gods
--
module NumberSix.Handlers.Gods
    ( handler
    ) where

import Data.List (nub, isInfixOf)

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
    prefix <- getPrefix
    modifyGods (add $ God prefix) password
    printGods
  where
    add x = nub . (x :)

removeGodHook :: Irc String ()
removeGodHook = onBangCommand "!removegod" $ onGod $ do
    password <- getGodPassword
    pattern <- getBangCommandText
    prefix <- getPrefix
    let keep = if null pattern then (/= God prefix) else noMatch pattern
    modifyGods (filter keep) password
    printGods
  where
    noMatch pattern god = not $ pattern `isInfixOf` show god

printGods :: Irc String ()
printGods = do
    gods <- getGods
    write $ "My gods are " <> prettyList (map show gods) <> "."
