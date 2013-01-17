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
import           Data.Char             (isDigit)


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
    , movieYear   :: ByteString
    , movieRating :: ByteString
    , movieVotes  :: ByteString
    , movieId     :: ByteString
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
parseQuery :: ByteString -> (ByteString, Maybe ByteString)
parseQuery bs = case reverse (BC.words bs) of
    (year : title)
        | BC.all isDigit year -> (BC.unwords (reverse title), Just year)
        | otherwise           -> (bs, Nothing)
    _                         -> (bs, Nothing)


--------------------------------------------------------------------------------
imdb :: ByteString -> IO ByteString
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
