-- | Provides term lookup on urbandictionary.com
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.UrbanDictionary
    ( urban
    , handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans  (liftIO)
import           Data.Text            (Text)
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
urban :: Text -> IO Text
urban query = do
    result <- httpScrape Html url id $ \cursor -> do
        def <- findRec (byTagNameAttrs "div" [("class", "meaning")]) cursor
        return $ removeNewlines $ nodeText $ current def
    maybe randomError return result
  where
    url = "http://www.urbandictionary.com/define.php?term=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "UrbanDictionary" ["!urban"] $ liftIO . urban
