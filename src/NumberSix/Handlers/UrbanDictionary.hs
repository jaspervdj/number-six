-- | Provides term lookup on urbandictionary.com
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.UrbanDictionary
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import           Text.HTML.TagSoup


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
urban :: ByteString -> IO ByteString
urban query = httpScrape url $ innerText . insideTag "div" .
    dropWhile (~/= TagOpen ("div" :: ByteString) [("class", "definition")])
  where
    url = "http://www.urbandictionary.com/define.php?term=" <> urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "urbandictionary" ["!urban"] $ \query -> liftIO $ do
    result <- urban query
    return $ case result of
        "" -> "Not found"
        x  -> x
