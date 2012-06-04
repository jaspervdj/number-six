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
import qualified Data.ByteString.Char8 as BC


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data HackerNews = HackerNews [Item] deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON HackerNews where
    parseJSON (Object o) = resolveSelfPosts . HackerNews <$> o .: "items"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
-- | Make an URLs in items absolute if needed
--
-- The HN api returns
--
-- > /comments/1234
--
-- as URL for these links, and we need
--
-- > http://news.ycombinator.com/item?id=1234
resolveSelfPosts :: HackerNews -> HackerNews
resolveSelfPosts (HackerNews items) = HackerNews $ map resolve items
  where
    resolve (Item title url) = Item title $ case (BC.split '/' url) of
        ["", "comments", nr] -> "http://news.ycombinator.com/item?id=" <> nr
        _                    -> url


--------------------------------------------------------------------------------
data Item = Item ByteString ByteString deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Item where
    parseJSON (Object o) = Item <$> o .: "title" <*> o .: "url"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
hackerNews :: ByteString -> IO ByteString
hackerNews query = do
    json <- http "api.ihackernews.com/page" id
    case parseJsonEither json of
        Right (HackerNews items) ->
            let Item title url = items !! idx
            in textAndUrl title url
        _ -> randomError
  where
    idx = case readByteString query of
        Just n -> n - 1
        _      -> 0


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "HackerNews" ["!hn"] $ liftIO . hackerNews
