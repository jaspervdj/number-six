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
import NumberSix.Util.Time

handler :: Handler ByteString
handler = makeHandler "seen" [storeHook, loadHook]

storeHook :: Irc ByteString ()
storeHook = onCommand "PRIVMSG" $ do
    sender <- getSender
    time <- getTime
    text <- getMessageText
    let lastSeen = (time, text)
    withRedis $ \redis -> setItem redis sender lastSeen

loadHook :: Irc ByteString ()
loadHook = onBangCommand "!seen" $ do
    (who, _) <- breakWord <$> getBangCommandText
    item <- withRedis $ \redis -> getItem redis who
    case item of
        Just (time, text) -> do
            pretty <- prettyTime time
            writeReply $ "I last saw " <> who <> " " <> pretty
                <> " saying: " <> text
        _ -> writeReply $ "I ain't never seen " <> who
