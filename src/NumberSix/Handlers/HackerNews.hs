-- | Link to hacker new items (<http://news.ycombinator.com>). This plugin uses
-- the API provided at <http://api.ihackernews.com/>
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.HackerNews
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON (..), Value (..), (.:))
import           Data.ByteString      (ByteString)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.BitLy
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data HackerNews = HackerNews [Item] deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON HackerNews where
    parseJSON (Object o) = HackerNews <$> o .: "items"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
data Item = Item ByteString ByteString deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Item where
    parseJSON (Object o) = Item <$> o .: "title" <*> o .: "url"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
hackerNews :: ByteString -> IO ByteString
hackerNews query = do
    json <- httpGet "api.ihackernews.com/page"
    case parseJsonEither json of
        Right (HackerNews items) ->
            let Item title url = items !! idx
            in textAndUrl title url
        _ -> return "Something went wrong"
  where
    idx = case readByteString query of
        Just n -> n - 1
        _      -> 0


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "hackernews" ["!hn"] $ liftIO . hackerNews
