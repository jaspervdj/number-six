--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Twitter
    ( twitter
    , handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.Char            (isDigit)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified Network.HTTP.Conduit as HC


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
newtype User = User Text


--------------------------------------------------------------------------------
instance FromJSON User where
    parseJSON (Object o) = User <$> o .: "screen_name"
    parseJSON _ = mzero


--------------------------------------------------------------------------------
data Tweet = Tweet User Text


--------------------------------------------------------------------------------
instance FromJSON Tweet where
    parseJSON (Object o) = Tweet <$> o .: "user" <*> o .: "text"
    parseJSON (Array a) = if V.null a then mzero else parseJSON (V.head a)
    parseJSON _ = mzero


--------------------------------------------------------------------------------
twitterApiKey :: ByteString
twitterApiKey = "AAAAAAAAAAAAAAAAAAAAAO5eRQAAAAAAdBaUWq2vDVmrhUaPN05c4wr2J3c%3D\
                \SnwTbpHpvxoD53jDRPDegvpUbzlfzJHKDjYJBlQ"


--------------------------------------------------------------------------------
getTweet :: ByteString -> IO Text
getTweet bs = case parseJsonEither bs of
    Left  _                        -> randomError
    Right (Tweet (User user) text) -> return $ removeNewlines $
        if "RT " `T.isPrefixOf` text then text else "@" <> user <> ": " <> text


--------------------------------------------------------------------------------
twitter :: Text -> IO Text
twitter argument
    | T.all isDigit argument = getTweet =<< http (tweet argument) auth
    | T.all isDigit fromUrl  = getTweet =<< http (tweet fromUrl)  auth
    | otherwise              = getTweet =<< http (user argument)  auth
  where
    fromUrl = T.reverse $ T.takeWhile (/= '/') $ T.reverse argument
    auth req = let header = ("Authorization", "Bearer " <> twitterApiKey)
               in req { HC.requestHeaders = header : HC.requestHeaders req }

    user u = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
           <> urlEncode u <> "&include_rts=1&count=1"
    tweet t =  "https://api.twitter.com/1.1/statuses/show/"
            <> urlEncode t <> ".json"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Twitter" ["!twitter"] $ liftIO . twitter
