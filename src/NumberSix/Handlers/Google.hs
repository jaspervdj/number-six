--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Google
    ( google
    , handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON (..), Value (..), (.:))
import           Data.Text            (Text)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
googleApiKey :: Text
googleApiKey = "AIzaSyBIYwie0BY-Txs92F5196V7iZb5Xn3cMxw"


--------------------------------------------------------------------------------
googleCseId :: Text
googleCseId = "015170067376393226585:deqxftulnbm"


--------------------------------------------------------------------------------
data Result = Result [Item] deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Result where
    parseJSON (Object o) = Result <$> o .: "items"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
data Item = Item Text Text deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Item where
    parseJSON (Object o) = Item <$> o .: "title" <*> o .: "link"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
-- | Returns the URL of the first found link
google :: Text -> IO Text
google query = do
    json <- http url id
    case parseJsonEither json of
        Right (Result (Item title link : _)) -> textAndUrl title link
        _                                    -> randomError
  where
    url = "https://www.googleapis.com/customsearch/v1" <>
        "?q=" <> urlEncode query <>
        "&key=" <> googleApiKey <>
        "&cx=" <> googleCseId <>
        "&alt=json"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Google" ["!google", "!g"] $ liftIO . google
