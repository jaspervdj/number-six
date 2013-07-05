{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Twitter
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<*>))
import           Control.Monad         (mzero)
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Char             (isDigit)
import qualified Data.Vector           as V


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data User = User ByteString


--------------------------------------------------------------------------------
instance FromJSON User where
    parseJSON (Object o) = User <$> o .: "screen_name"
    parseJSON _ = mzero


--------------------------------------------------------------------------------
data Tweet = Tweet User ByteString


--------------------------------------------------------------------------------
instance FromJSON Tweet where
    parseJSON (Object o) = Tweet <$> o .: "user" <*> o .: "text"
    parseJSON (Array a) = if V.null a then mzero else parseJSON (V.head a)
    parseJSON _ = mzero


--------------------------------------------------------------------------------
getTweet :: ByteString -> IO ByteString
getTweet bs = case parseJsonEither bs of
    Left  _                        -> randomError
    Right (Tweet (User user) text) -> return $ removeNewlines $
        if "RT " `B.isPrefixOf` text then text else "@" <> user <> ": " <> text


--------------------------------------------------------------------------------
twitter :: ByteString -> IO ByteString
twitter argument
    | B.all isDigit argument = getTweet =<< http (tweet argument) id
    | B.all isDigit fromUrl  = getTweet =<< http (tweet fromUrl)  id
    | otherwise              = getTweet =<< http (user argument)  id
  where
    fromUrl = B.reverse $ B.takeWhile (/= '/') $ B.reverse argument

    user u = "http://api.twitter.com/1/statuses/user_timeline.json?screen_name="
           <> urlEncode u <> "&include_rts=1&count=1"
    tweet t =  "http://api.twitter.com/1/statuses/show/"
            <> urlEncode t <> ".json"


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Twitter" ["!twitter"] $ liftIO . twitter
