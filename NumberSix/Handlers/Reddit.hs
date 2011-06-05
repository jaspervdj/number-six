{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Reddit
    ( handler
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.Map as M
import Data.Aeson (FromJSON (..), Object, Value (..), (.:))

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.BitLy
import NumberSix.Util.Http

data Reddit = Reddit [Link] deriving (Show)

instance FromJSON Reddit where
    parseJSON (Object o) = let o' = unData o in Reddit <$> o' .: "children"
    parseJSON _          = mzero

data Link = Link ByteString ByteString deriving (Show)

instance FromJSON Link where
    parseJSON (Object o) =
        let o' = unData o in Link <$> o' .: "title" <*> o' .: "url" 
    parseJSON _          = mzero

-- | Fetch the data attribute from an object. Reddit's ugly json...
unData :: Object -> Object
unData o = case M.lookup "data" o of Just (Object o') -> o'; _ -> M.empty

reddit :: ByteString -> Irc ByteString
reddit query = httpGet url >>= \bs -> case parseJsonEither bs of
        Left _ -> return "Reddit is down, keep refreshing!"
        Right (Reddit l) -> randomElement l >>= textAndUrl'
  where
    url = "http://reddit.com/r/" <> query <> ".json"
    textAndUrl' (Link t u) = textAndUrl t u

handler :: Handler
handler = makeBangHandler "reddit" ["!reddit"] reddit
