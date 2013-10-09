-- | Takes the last item from a GitHub activity feed
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.GitHub
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
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
gitHub :: Text -> IO Text
gitHub query = do
    result <- httpScrape Xml url id $ \cursor -> do
        entry <- findChild (byTagName "entry") cursor
        title <- findChild (byTagName "title") entry
        link  <- findRec   (byTagName "link") entry
        href  <- getAttribute "href" $ current link
        let text = nodeText $ current title
        return (text, href)
    case result of
        Just (text, href) -> textAndUrl text href
        Nothing           -> randomError
  where
    url = "http://github.com/" <> urlEncode query <> ".atom"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "GitHub" ["!github"] $ liftIO . gitHub
