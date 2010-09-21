{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Seen
    ( handler
    ) where

import Control.Applicative ((<$>))

import Data.ByteString (ByteString)

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Redis

handler :: Handler ByteString
handler = makeHandler "seen" [storeHook, loadHook]

storeHook :: Irc ByteString ()
storeHook = onCommand "privmsg" $ do
    sender <- getSender
    time <- prettyTime
    text <- getMessageText
    let lastSeen = (time, text)
    withRedis $ \redis -> setItem redis sender lastSeen

loadHook :: Irc ByteString ()
loadHook = onBangCommand "!seen" $ do
    (who, _) <- breakWord <$> getBangCommandText
    item <- withRedis $ \redis -> getItem redis who
    case item of
        Just (time, text) -> writeChannelReply $
            "I last saw " <> who <> " on " <> time
                          <> " saying: " <> text
        _ -> writeChannelReply $ "I ain't never seen " <> who
