{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Sup
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import qualified Data.ByteString as B
import           Data.List (intercalate)
import qualified Data.Map as M

import NumberSix.Irc
import NumberSix.Message

handler :: UninitializedHandler
handler = makeHandler "AlfaPapaTango" [alfaPapaTangoHook]

alfaPapaTangoHook :: Irc ()
alfaPapaTangoHook = onCommand "PRIVMSG" $ do
    text <- getMessageText
    sender <- getSender
    let replyText = map translate text
    write $ sender <> " " <> replyText

letterMap = M.fromList $ zip [a..z] $
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

translate :: String -> String
translate letters = 
    let ws = map (\c -> case Map.lookup c letterMap of 
                            Just s -> s
                            Nothing -> [c]) letters
    in intercalate (" " :: String) ws
