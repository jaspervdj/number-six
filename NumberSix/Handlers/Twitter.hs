module NumberSix.Handlers.Twitter
    ( handler
    ) where

import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Control.Monad (mplus)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Http

getTweet :: Maybe String -> [Tag String] -> String
getTweet muser tags = fromMaybe "Not found" $ do
    text <- nextTagText tags "text"
    user <- muser `mplus` nextTagText tags "screen_name"
    return $ "@" <> user <> ": " <> removeNewlines text

twitter :: String -> Irc String String
twitter argument = if all isDigit argument
    then httpScrape tweet $ getTweet Nothing
    else httpScrape user $ getTweet $ Just argument
  where
    user  =  "http://api.twitter.com/1/statuses/user_timeline.xml?screen_name="
          <> urlEncode argument <> "&include_rts=1&count=1"
    tweet =  "http://api.twitter.com/1/statuses/show/"
          <> urlEncode argument <> ".xml"

handler :: Handler String
handler = makeBangHandler "twitter" ["!twitter"] twitter
