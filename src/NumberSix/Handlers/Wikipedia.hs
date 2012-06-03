--------------------------------------------------------------------------------
-- | Wikipedia lookup handler
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Wikipedia
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans  (liftIO)
import           Data.ByteString      (ByteString)
import qualified Data.Text.Encoding   as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
wiki :: ByteString -> IO ByteString
wiki query = do
    result <- httpGetScrape Xml url $ \cursor -> do
        item  <- findRec   (byTagName "Item")        cursor
        descr <- findChild (byTagName "Description") item
        return $ T.encodeUtf8 $ nodeText $ current descr

    maybe randomError return result
  where
    url = "http://en.wikipedia.org/w/api.php" <>
        "?action=opensearch" <>
        "&format=xml" <>
        "&search=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Wikipedia" ["!w", "!wik", "!wiki"] $ liftIO . wiki
