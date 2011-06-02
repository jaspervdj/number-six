{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Twitter
    ( handler
    ) where

import Data.Char (isDigit)
import Data.ByteString (ByteString)
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as B

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Http

getTweet :: [Tag ByteString] -> ByteString
getTweet tags =
    let text = removeNewlines $ innerText $ insideTag "text" tags
        user = innerText $ insideTag "screen_name" tags
    in if B.null text || B.null user
        then "Not found"
        else if "RT " `B.isPrefixOf` text
            then text
            else "@" <> user <> ": " <> text

twitter :: ByteString -> Irc ByteString
twitter argument
    | B.all isDigit argument = httpScrape (tweet argument) getTweet
    | B.all isDigit fromUrl  = httpScrape (tweet fromUrl) getTweet
    | otherwise = httpScrape (user argument) getTweet
  where
    fromUrl = B.reverse $ B.takeWhile (/= '/') $ B.reverse argument

    user u = "http://api.twitter.com/1/statuses/user_timeline.xml?screen_name="
           <> urlEncode u <> "&include_rts=1&count=1"
    tweet t =  "http://api.twitter.com/1/statuses/show/"
            <> urlEncode t <> ".xml"

handler :: Handler
handler = makeBangHandler "twitter" ["!twitter"] twitter
