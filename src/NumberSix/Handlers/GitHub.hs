-- | Takes the last item from a GitHub activity feed
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.GitHub
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
import           NumberSix.Util.BitLy
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
gitHub :: ByteString -> IO ByteString
gitHub query = do
    result <- httpGetHtmlScrape url $ \cursor -> do
        entry <- findChild (byTagName "entry") cursor
        title <- findChild (byTagName "title") entry
        link  <- findRec (byTagName "link") entry
        href  <- getAttribute "href" $ current link
        let text = nodeText $ current title
        return (T.encodeUtf8 text, T.encodeUtf8 href)
    case result of
        Just (text, href) -> textAndUrl text href
        Nothing           -> return "Malformed data!"
  where
    url = "http://github.com/" <> urlEncode query <> ".atom"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "github" ["!github"] $ liftIO . gitHub
