-- | Provides URL shortening through the bit.ly API
--
module NumberSix.Util.BitLy
    ( shorten
    , textAndUrl
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import NumberSix.Irc
import NumberSix.Util.Http

shorten :: String -> Irc String
shorten query = fromMaybe query <$> httpScrape SimpleHttp url getUrl
  where
    getUrl tags = nextTagText tags "url"
    url =  "http://api.bit.ly/v3/shorten?login=jaspervdj"
        ++ "&apiKey=R_578fb5b17a40fa1f94669c6cba844df1"
        ++ "&longUrl=" ++ urlEncode query
        ++ "&format=xml"

textAndUrl :: String -> String -> Irc String
textAndUrl text url = do
    shortUrl <- shorten url
    return $ text ++ " >> " ++ shortUrl
