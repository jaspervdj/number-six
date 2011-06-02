{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Reddit
    ( handler
    ) where

import Control.Monad (forM)
import Text.JSON

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.BitLy
import NumberSix.Util.Http

reddit :: ByteString -> Irc ByteString
reddit query = do
    Ok (JSObject root) <- fmap (decode . B.unpack) $ httpGet url
    let Ok urls = do
            JSObject data_ <- valFromObj "data" root
            JSArray children <- valFromObj "children" data_
            forM children $ \(JSObject child) -> do
                JSObject childData <- valFromObj "data" child
                url' <- valFromObj "url" childData
                title <- valFromObj "title" childData
                return (B.pack $ fromJSString title, B.pack $ fromJSString url')
    randomElement urls >>= uncurry textAndUrl
  where
    url = "http://reddit.com/r/" <> query <> ".json"

handler :: Handler
handler = makeBangHandler "reddit" ["!reddit"] reddit
