--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Weather
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans  (liftIO)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
weather :: Text -> IO Text
weather query = do
    result <- httpScrape Html url id $ \cursor -> do
        -- Find content div
        con    <- findRec (byTagNameAttrs "div" [("class", "content")]) cursor
        temp   <- findRec (byTagNameAttrs "span" [("class", "temperature")]) con
        remark <- findRec (byTagNameAttrs "p" [("class", "remark")]) con
        return $ (nodeText $ current temp) <>
            "°?! " <>
            (nodeText $ current remark)

    maybe randomError return result
  where
    loc = if T.null query then "ghent" else query
    url = "http://thefuckingweather.com/?unit=c&where=" <> loc


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Weather" ["!weather"] $ liftIO . weather
