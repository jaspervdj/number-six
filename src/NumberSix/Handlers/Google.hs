{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Google
    ( handler
    , google
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import           Data.List           (find)
import           Text.HTML.TagSoup


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
-- | Returns the URL of the first found link
google :: ByteString -> IO ByteString
google query = httpScrape url $ \tags ->
    let Just (TagOpen _ attrs) =
            find (~== TagOpen ("a" :: ByteString) [("class", "l")]) tags
        Just t = lookup "href" attrs
    in t
  where
    url = "http://www.google.com/search?q=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "google" ["!google", "!g"] $ liftIO . google
