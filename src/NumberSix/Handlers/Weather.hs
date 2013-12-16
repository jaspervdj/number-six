--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Weather
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON (..), Value (..), (.:))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.XmlHtml
import           Text.XmlHtml.Cursor


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
kelvin = 273.15

--------------------------------------------------------------------------------
data Weather = Weather Temperature [Description] deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Weather where
    parseJSON (Object o) = Weather
                            <$> (o .: "main")
                            <*> (o .: "weather")
    parseJSON _          = mzero


--------------------------------------------------------------------------------
data Temperature = Temperature Float deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Temperature where
    parseJSON (Object o) = Temperature <$> (flip (-) $ kelvin) <$> o .: "temp"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
data Description = Description Text deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Description where
    parseJSON (Object o) = Description <$> o .: "description"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
weather :: Text -> IO Text
weather query = do
    result <- (parseJsonEither <$> http url id) :: IO (Either String Weather)
    either (const randomError) (return . T.pack . show) result
  where
    loc = if T.null query then "gent" else query
    url = "http://api.openweathermap.org/data/2.5/weather?q=" <> loc


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Weather" ["!weather"] $ liftIO . weather
