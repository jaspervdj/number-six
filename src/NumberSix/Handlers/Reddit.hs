{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Reddit
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<*>))
import           Control.Monad         (mzero)
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (FromJSON (..), Object, Value (..), (.:))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy     as HM


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.BitLy
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data Reddit = Reddit [Link] deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Reddit where
    parseJSON (Object o) = let o' = unData o in Reddit <$> o' .: "children"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
data Link = Link ByteString ByteString deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Link where
    parseJSON (Object o) =
        let o' = unData o in Link <$> o' .: "title" <*> o' .: "url" 
    parseJSON _          = mzero


--------------------------------------------------------------------------------
-- | Fetch the data attribute from an object. Reddit's ugly json...
unData :: Object -> Object
unData o = case HM.lookup "data" o of Just (Object o') -> o'; _ -> HM.empty


--------------------------------------------------------------------------------
reddit :: ByteString -> IO ByteString
reddit query = http url id >>= \bs -> case parseJsonEither bs of
    Left  _           -> randomError
    Right (Reddit ls) -> do
        Link t u <- case idx of
            Nothing -> randomElement ls
            Just i  -> return $ ls !! (i - 1)
        textAndUrl t u
  where
    url              = "http://reddit.com/r/" <> subreddit <> ".json"
    (subreddit, idx) = case BC.words query of
        [s, i] -> (s, readByteString i)
        [s]    -> case readByteString s of
            Just i  -> ("all", Just i)
            Nothing -> (s, Nothing)
        _      -> ("all", Nothing)


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Reddit" ["!reddit"] $ liftIO . reddit
