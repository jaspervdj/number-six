--------------------------------------------------------------------------------
-- | Wikipedia lookup handler
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Wikipedia
    ( handler
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
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
wiki :: Text -> IO Text
wiki query = do
    result <- httpScrape Xml url id $ \cursor -> do
        item  <- findRec   (byTagName "Item")        cursor
        descr <- findChild (byTagName "Description") item
        return $ nodeText $ current descr

    maybe randomError return result
  where
    url = "http://en.wikipedia.org/w/api.php" <>
        "?action=opensearch" <>
        "&format=xml" <>
        "&search=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Wikipedia" ["!w", "!wik", "!wiki"] $ liftIO . wiki
