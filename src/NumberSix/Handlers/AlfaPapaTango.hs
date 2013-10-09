--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.AlfaPapaTango
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Data.Char      (toLower)
import qualified Data.Map       as M
import           Data.Maybe     (mapMaybe)
import           Data.Text      (Text)
import qualified Data.Text      as T


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandler "AlfaPapaTango" [alfaPapaTangoHook]


--------------------------------------------------------------------------------
alfaPapaTangoHook :: Irc ()
alfaPapaTangoHook = onBangCommand "!nato" $ do
    text <- getBangCommandText
    writeReply $ translate text


--------------------------------------------------------------------------------
letterMap :: M.Map Char Text
letterMap = M.fromList $
    [ (T.head w, w)
    | w <-
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
    ]


--------------------------------------------------------------------------------
translate :: Text -> Text
translate = T.unwords .
    mapMaybe (\c -> M.lookup (toLower c) letterMap) . T.unpack
