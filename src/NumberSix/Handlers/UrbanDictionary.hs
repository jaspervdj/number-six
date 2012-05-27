-- | Provides term lookup on urbandictionary.com
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.UrbanDictionary
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import qualified Data.Text.Encoding  as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
urban :: ByteString -> IO ByteString
urban query = do
    result <- httpGetScrape Html url $ \cursor -> do
        def <- findRec (byTagNameAttrs "div" [("class", "definition")]) cursor
        return $ T.encodeUtf8 $ nodeText $ current def
    maybe randomError return result
  where
    url = "http://www.urbandictionary.com/define.php?term=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "UrbanDictionary" ["!urban"] $ liftIO . urban
