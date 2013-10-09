-- | Get a tumblr posts for a given user
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Tumblr
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<*>))
import           Control.Monad         (mzero)
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (FromJSON (..), Value (..), (.:))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Text             (Text)
import qualified Data.Text             as T


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.BitLy
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data Tumblr = Tumblr [Post] deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Tumblr where
    parseJSON (Object o) = Tumblr <$> o .: "posts"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
data Post = Post Text Text deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Post where
    parseJSON (Object o) = Post <$> o .: "url" <*> o .: "slug"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
-- | Tumblr gives a string containing the JSON value, but assign it to a
-- variable in a JavaScript like approach, defining the variable
-- tumblr_api_read to use further in other JS scripts on the caling page.
-- Obviously, this approach is pretty detrimental for our purpose, so we need
-- to clean the returned string before passing it on to the JSON decoder.
cleanTumblrJSON :: ByteString -> ByteString
cleanTumblrJSON str
    | tumblrPrefix `B.isPrefixOf` str = B.reverse $ B.tail $
        B.dropWhile (/= ';') $ B.reverse $ B.drop (B.length tumblrPrefix) str
    | otherwise = ""
  where
    tumblrPrefix = "var tumblr_api_read = "


--------------------------------------------------------------------------------
-- | Get a random tumble from the user, taken from the last 'count' tumbles. To
-- obtain the last tumble, just pass 1 as the count.
randomTumble :: Text -> Int -> IO Text
randomTumble query count = do
    result <- parseJsonEither . cleanTumblrJSON <$> http url id
    case result of
        Right (Tumblr posts) -> do
            Post url' slug <- randomElement posts
            textAndUrl slug url'
        Left e               ->
            return $ "Something went wrong: " <> T.pack e
  where
    url = "http://" <> query <> ".tumblr.com/api/read/json?num=" <>
        T.pack (show count)


--------------------------------------------------------------------------------
tumblr :: Text -> IO Text
tumblr query =
    let (command : user) = T.words query
    in case command of
        "last" -> randomTumble (head user) 1
        _      -> randomTumble query 50


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Tumblr" ["!tumblr"] $ liftIO . tumblr
