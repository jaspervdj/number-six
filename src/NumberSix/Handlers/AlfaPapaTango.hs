{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.AlfaPapaTango
    ( handler
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import NumberSix.Irc
import NumberSix.Message

handler :: UninitializedHandler
handler = makeHandler "AlfaPapaTango" [alfaPapaTangoHook]

alfaPapaTangoHook :: Irc ()
alfaPapaTangoHook = onCommand "PRIVMSG" $ do
    text <- getMessageText
    sender <- getSender
    let replyText = translate text
    write $ sender <> replyText

letterMap :: M.Map Char BC.ByteString
letterMap = M.fromList $ zip ['a'..'z'] $ map BC.pack
    [ "alfa"
    , "bravo"
    , "charlie"
    , "delta"
    , "echo"
    , "foxtrot"
    , "golf"
    , "hotel"
    , "india"
    , "julliett"
    , "kilo"
    , "lima"
    , "mike"
    , "november"
    , "oscar"
    , "papa"
    , "quebec"
    , "romeo"
    , "sierra"
    , "tango"
    , "uniform"
    , "victor"
    , "whiskey"
    , "xray"
    , "yankee"
    , "zulu"
    ]

translate :: BC.ByteString -> BC.ByteString
translate letters = BC.concatMap (\c -> case M.lookup c letterMap of 
                                            Just s -> s
                                            Nothing -> BC.pack [c]) letters
