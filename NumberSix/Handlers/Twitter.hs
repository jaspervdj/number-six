module NumberSix.Handlers.Twitter
    ( handler
    ) where

import Text.HTML.TagSoup

import NumberSix.Irc
import NumberSix.Util.Http

twitter :: String -> Irc String
twitter userName = httpScrape url $ \tags ->
    case dropWhile (~/= TagOpen "text" []) tags of
        (_ : TagText t : _) -> "@" ++ userName ++ ": " ++ t
        _ -> "Not found"
  where
    url =  "http://api.twitter.com/1/statuses/user_timeline.xml?screen_name="
        ++ urlEncode userName

handler :: Handler
handler = makeHandler "twitter" $ onBangCommand "!twitter" $
    getBangCommandText >>= twitter >>= writeChannel
