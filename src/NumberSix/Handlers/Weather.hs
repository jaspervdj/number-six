{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Weather
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans  (liftIO)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
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
weather :: ByteString -> IO ByteString
weather query = do
    result <- httpGetScrape Html url $ \cursor -> do
        -- Find content div
        con    <- findRec (byTagNameAttrs "div" [("class", "content")]) cursor
        temp   <- findRec (byTagNameAttrs "span" [("class", "temperature")]) con
        remark <- findRec (byTagNameAttrs "p" [("class", "remark")]) con
        return $ T.encodeUtf8 (nodeText $ current temp) <>
            T.encodeUtf8 "°?! " <>
            T.encodeUtf8 (nodeText $ current remark)

    maybe randomError return result
  where
    loc = if B.null query then "ghent" else query
    url = "http://thefuckingweather.com/?unit=c&where=" <> loc


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "weather" ["!weather"] $ liftIO . weather
