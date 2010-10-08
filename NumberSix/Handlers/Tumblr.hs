-- | Get a tumblr posts for a given user
--
module NumberSix.Handlers.Tumblr
    ( handler
    ) where

import Control.Monad(forM, liftM)
import Data.Char(isSpace)
import Data.List(dropWhile, stripPrefix)
import Text.JSON

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.BitLy
import NumberSix.Util.Http


-- | Tumblr gives a string containing the JSON value, but assign it to a
-- variable in a JavaScript like approach, defining the variable
-- tumblr_api_read to use further in other JS scripts on the caling page.
-- Obviously, this approach is pretty detrimental for our purpose, so we need
-- to clean the returned string before passing it on to the JSON decoder.
-- 
cleanTumblrJSON :: String -> Maybe String
cleanTumblrJSON xs = stripPrefix tumblrPrefix xs >>= Just . reverse . tail . dropWhile (/= ';') . reverse
  where tumblrPrefix = "var tumblr_api_read = "

--
-- | Get a random tumble from the user, taken from the last 'count' tumbles. To
-- obtain the last tumble, just pass 1 as the count.
--
randomTumble :: String -> Int -> Irc String String
randomTumble query count = do
  result <- (decode . (\s -> case cleanTumblrJSON s of
                              Just s' -> s'
                              Nothing -> "")) `fmap` httpGet url :: Irc String (Result JSValue)
  case result of
      Ok (JSObject root) -> let Ok dest = do JSArray posts <- valFromObj "posts" root
                                             forM posts $ \(JSObject post) -> do
                                                url' <- valFromObj "url" post
                                                slug <- valFromObj "slug" post
                                                return (fromJSString slug, fromJSString url')
                            in randomElement dest >>= uncurry textAndUrl
      Error s -> textAndUrl ("Something went wrong when fetching " ++ url) ""
  where url = "http://" ++ query ++ ".tumblr.com/api/read/json?num=" ++ show count

tumblr :: String -> Irc String String
tumblr query = 
  let command:user = words query
  in case command of
        "last" -> randomTumble (head user) 1
        _ -> randomTumble query 50

handler :: Handler String
handler = makeBangHandler "tumblr" ["!tumblr"] tumblr

