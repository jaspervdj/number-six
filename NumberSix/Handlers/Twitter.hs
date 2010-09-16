module NumberSix.Handlers.Twitter
    ( handler
    ) where

import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Control.Monad (mplus)

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Http

getTweet :: Maybe ByteString -> [Tag ByteString] -> ByteString
getTweet muser tags = fromMaybe "Not found" $ do
    text <- nextTagText tags "text"
    user <- muser `mplus` nextTagText tags "screen_name"
    return $ "@" ++ user ++ ": " ++ text

twitter :: ByteString -> Irc ByteString
twitter argument = if all isDigit argument
    then httpScrape SimpleHttp tweet $ getTweet Nothing
    else httpScrape SimpleHttp user $ getTweet $ Just argument
  where
    user  =  "http://api.twitter.com/1/statuses/user_timeline.xml?screen_name="
          ++ urlEncode argument ++ "&include_rts=1"
    tweet =  "http://api.twitter.com/1/statuses/show/"
          ++ urlEncode argument ++ ".xml"

handler :: Handler
handler = makeBangHandler "twitter" ["!twitter"] twitter
