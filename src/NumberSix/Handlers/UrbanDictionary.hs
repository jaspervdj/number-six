-- | Provides term lookup on urbandictionary.com
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.UrbanDictionary
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text.Encoding  as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
urban :: ByteString -> IO ByteString
urban query = do
    result <- httpGetScrape Html url $ \cursor -> do
        def <- findRec (byTagNameAttrs "div" [("class", "definition")]) cursor
        return $ T.encodeUtf8 $ nodeText $ current def
    return $ fromMaybe "Look it up yourself" result
  where
    url = "http://www.urbandictionary.com/define.php?term=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "urbandictionary" ["!urban"] $ liftIO . urban
