-- | Handler that allows looking up video's on YouTube
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.YouTube
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>))
import           Control.Monad.Trans  (liftIO)
import           Data.ByteString      (ByteString)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
youTube :: ByteString -> IO ByteString
youTube query = do
    result <- httpGetScrape Xml url $ \cursor -> do
        -- Find entry and title, easy...
        entry <- findChild (byTagName "entry") cursor
        title <- nodeText . current <$> findChild (byTagName "title") entry

        -- Also drop the '&feature...' part from the URL
        link  <- findChild (byTagNameAttrs "link" [("rel", "alternate")]) entry
        href  <- T.takeWhile (/= '&') <$> getAttribute "href" (current link)

        return (T.encodeUtf8 title, T.encodeUtf8 href)
    case result of
        Just (text, href) -> textAndUrl text href
        Nothing           -> randomError
  where
    url = "http://gdata.youtube.com/feeds/api/videos?q=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "youtube" ["!youtube", "!y"] $ liftIO . youTube
