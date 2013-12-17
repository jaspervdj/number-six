--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Weather
    ( handler
    , get_weather
    , Weather (..)
    , Temperature (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Exception    (catch)
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON (..), Value (..), (.:))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Conduit (HttpException)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
kelvin :: Float
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
data Temperature = Temperature { temp :: Float }


--------------------------------------------------------------------------------
instance FromJSON Temperature where
    parseJSON (Object o) = Temperature <$> (flip (-) $ kelvin) <$> o .: "temp"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
instance Show Temperature where
    show (Temperature t) = show t ++ "°!?"


--------------------------------------------------------------------------------
data Description = Description { describe :: Text }


--------------------------------------------------------------------------------
instance FromJSON Description where
    parseJSON (Object o) = Description <$> o .: "description"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
instance Show Description where
    show (Description d) = T.unpack $ T.toUpper d


--------------------------------------------------------------------------------
get_weather :: Text -> IO (Maybe Weather)
get_weather query = do
    result <- ((parseJsonEither <$> http url id) :: IO (Either String Weather))
        `catch` (\ex -> return $ Left $ show (ex :: HttpException))
    either (const $ return Nothing) (return . Just) result
  where
    loc = if T.null query then "gent" else query
    url = "http://api.openweathermap.org/data/2.5/weather?q=" <> loc


--------------------------------------------------------------------------------
weather :: Text -> IO Text
weather query = do
    result <- get_weather query
    maybe randomError (return . pprint) result
  where
    pprint :: Weather -> Text
    pprint (Weather t ds) = T.pack (show $ (round $ temp t :: Int)) <> "°!?"
                            <> T.toUpper (go ds)
      where
        go []  = T.pack ""
        go [d] = " AND FUCK, " <> describe d
        go dss = " AND FUCK, "
                <> T.intercalate (T.pack ", ") (map describe $ init dss)
                <> " AND " <> (describe $ last dss)


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Weather" ["!weather"] $ liftIO . weather
