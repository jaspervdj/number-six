module NumberSix.Handlers.Twitter
    ( handler
    ) where

import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Util.Http

getTweet :: [Tag String] -> String
getTweet tags = fromMaybe "Not found" $ do
    text <- nextTagText tags "text"
    user <- nextTagText tags "screen_name"
    return $ "@" ++ user ++ ": " ++ text

twitter :: String -> Irc String
twitter argument =
    let url = if all isDigit argument then tweet else user
    in httpScrape url getTweet
  where
    user  =  "http://api.twitter.com/1/statuses/user_timeline.xml?screen_name="
          ++ urlEncode argument
    tweet =  "http://api.twitter.com/1/statuses/show/"
          ++ urlEncode argument ++ ".xml"

handler :: Handler
handler = makeBangHandler "twitter" "!twitter" $ fmap return . twitter
