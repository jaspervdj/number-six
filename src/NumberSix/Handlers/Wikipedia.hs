-- | Wikipedia lookup handler
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Wikipedia
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
wiki :: ByteString -> IO ByteString
wiki query = fmap (T.encodeUtf8 . fromMaybe "Not found") $
    httpGetScrape Html url $ \cursor -> do
        body  <- findRec (byTagNameAttrs "div" [("id", "bodyContent")]) cursor
        short <- nodeText . current <$> findRec (byTagName "p") body
        if " may refer to:" `T.isSuffixOf` short
            then nodeText . current <$> findRec (byTagName "li") body
            else return short
  where
    url =
        "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" <>
        urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "wikipedia" ["!w", "!wik", "!wiki"] $ liftIO . wiki
