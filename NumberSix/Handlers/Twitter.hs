module NumberSix.Handlers.Twitter
    ( handler
    ) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

import Text.HTML.TagSoup
import Text.Regex.PCRE

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Http

getTweet :: [Tag String] -> String
getTweet tags =
    let text = removeNewlines $ innerText $ insideTag "text" tags
        user = innerText $ insideTag "screen_name" tags
    in if null text || null user
        then "Not found"
        else if "RT " `isPrefixOf` text
            then text
            else "@" <> user <> ": " <> text

twitter :: String -> Irc String String
twitter argument
    | all isDigit argument = httpScrape (tweet argument) getTweet
    | argument =~ ".*/status/[0-9]+$" = httpScrape (tweet fromUrl) getTweet
    | otherwise = httpScrape (user argument) getTweet
  where
    fromUrl = reverse $ takeWhile (/= '/') $ reverse argument

    user u = "http://api.twitter.com/1/statuses/user_timeline.xml?screen_name="
           <> urlEncode u <> "&include_rts=1&count=1"
    tweet t =  "http://api.twitter.com/1/statuses/show/"
            <> urlEncode t <> ".xml"

handler :: Handler String
handler = makeBangHandler "twitter" ["!twitter"] twitter
