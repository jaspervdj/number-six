--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Imdb
    ( imdb
    , handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<*>))
import           Control.Monad         (mzero)
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (FromJSON (..), Value (..), (.:))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data Movie = Movie
    { movieTitle  :: ByteString
    , movieYear   :: Int
    , movieRating :: Double
    , movieUrl    :: ByteString
    } deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Movie where
    parseJSON (Object o) = Movie
        <$> o .: "title"
        <*> o .: "year"
        <*> o .: "rating"
        <*> o .: "imdb_url"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
imdb :: ByteString -> IO ByteString
imdb query = do
    json <- http url id
    case parseJsonEither json of
        Right (m : _) -> textAndUrl
            (movieTitle m <>
                " (" <> BC.pack (show $ movieYear m) <>
                "): " <> BC.pack (show $ movieRating m))
            (movieUrl m)
        _             -> randomError
  where
    url = "http://imdbapi.org/" <>
        "?title=" <> urlEncode query <>
        "&limit=1" <>
        "&type=json"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Imdb" ["!imdb"] $ liftIO . imdb
