--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Imdb
    ( imdb
    , handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON (..), Value (..), (.:))
import           Data.Char            (isDigit)
import           Data.Text            (Text)
import qualified Data.Text            as T


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
    { movieTitle  :: Text
    , movieYear   :: Text
    , movieRating :: Text
    , movieVotes  :: Text
    , movieId     :: Text
    } deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Movie where
    parseJSON (Object o) = Movie
        <$> o .: "Title"
        <*> o .: "Year"
        <*> o .: "imdbRating"
        <*> o .: "imdbVotes"
        <*> o .: "imdbID"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
-- | Parse optional trailing year
parseQuery :: Text -> (Text, Maybe Text)
parseQuery bs = case reverse (T.words bs) of
    (year : title)
        | T.all isDigit year -> (T.unwords (reverse title), Just year)
        | otherwise          -> (bs, Nothing)
    _                        -> (bs, Nothing)


--------------------------------------------------------------------------------
imdb :: Text -> IO Text
imdb query = do
    json <- http url id
    case parseJsonEither json of
        Right m -> textAndUrl
            (movieTitle m <> " ("
                <> movieYear m <> "): " <>
                movieRating m <> " (" <>
                movieVotes m <> " votes)")
            ("http://www.imdb.com/title/" <> movieId m <> "/")
        _       -> randomError
  where
    (title, myear) = parseQuery query
    url            =
        "http://omdbapi.com/" <>
            "?t=" <> urlEncode title <>
            maybe "" ("&y=" <>) myear


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Imdb" ["!imdb"] $ liftIO . imdb
