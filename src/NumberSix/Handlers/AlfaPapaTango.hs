{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.AlfaPapaTango
    ( handler
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as DC (toLower)
import qualified Data.Map as M

import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message

handler :: UninitializedHandler
handler = makeHandler "AlfaPapaTango" [alfaPapaTangoHook]

alfaPapaTangoHook :: Irc ()
alfaPapaTangoHook = onBangCommand "!nato" $ do
    text <- getBangCommandText
    sender <- getSender
    let replyText = translate text
    write $ sender <> replyText

letterMap :: M.Map Char BC.ByteString
letterMap = M.fromList $ zip ['a'..'z'] $ map BC.pack
    [ " alfa"
    , " bravo"
    , " charlie"
    , " delta"
    , " echo"
    , " foxtrot"
    , " golf"
    , " hotel"
    , " india"
    , " julliett"
    , " kilo"
    , " lima"
    , " mike"
    , " november"
    , " oscar"
    , " papa"
    , " quebec"
    , " romeo"
    , " sierra"
    , " tango"
    , " uniform"
    , " victor"
    , " whiskey"
    , " xray"
    , " yankee"
    , " zulu"
    ]

translate :: BC.ByteString -> BC.ByteString
translate letters = BC.tail $ BC.concatMap (\c -> case M.lookup (DC.toLower c) letterMap of 
                                                    Just s -> s
                                                    Nothing -> BC.pack [' ',c]) letters
