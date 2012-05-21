-- | Takes the last item from a GitHub activity feed
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.GitHub
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans  (liftIO)
import           Data.ByteString      (ByteString)
import           Data.List            (find)
import           Text.HTML.TagSoup


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.BitLy
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
gitHub :: ByteString -> IO ByteString
gitHub query = do
    (text, longUrl) <- httpScrape url $ \tags ->
        let e = insideTag "entry" tags
            text = innerText $ insideTag "title" e
            Just (TagOpen _ a) = find (~== TagOpen ("link" :: ByteString) []) e
            Just url' = lookup "href" a
        in (text, url')
    textAndUrl text longUrl
  where
    url = "http://github.com/" <> urlEncode query <> ".atom"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "github" ["!github"] $ liftIO . gitHub
