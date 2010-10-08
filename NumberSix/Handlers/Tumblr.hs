-- | Get a tumblr posts for a given user
--
module NumberSix.Handlers.Tumblr
    ( handler
    ) where

import Control.Monad(forM, liftM)
import Data.Char(isSpace)
import Data.List(stripPrefix)
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
cleanTumblrJSON xs = stripPrefix tumblrPrefix xs >>= Just . reverse . tail . dropUntil (== ';') . reverse
  where tumblrPrefix = "var tumblr_api_read = "


dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f xss@(x:xs)
  | f x  = xss
  | otherwise = dropUntil f xs



-- | Get the last tumbl from the user specified in the query
--
lastTumble :: String -> Irc String String
lastTumble query = do
  result <- (decode . (\s -> case cleanTumblrJSON s of
                              Just s' -> s'
                              Nothing -> "")) `fmap` httpGet url :: Irc String (Result JSValue)
  case result of
    Ok (JSObject root) -> let Ok dest = do JSArray (JSObject post:_) <- valFromObj "posts" root
                                           url' <- valFromObj "url" post
                                           slug <- valFromObj "slug" post
                                           return (fromJSString slug, fromJSString url')
                          in uncurry textAndUrl dest
    Error s -> textAndUrl ("Oops!" ++ s) url
  where url = "http://" ++ query ++ ".tumblr.com/api/read/json?num=1"
--
-- | Get a random tumbl from the user, taken from the last 50 tumbls
--
randomTumble :: String -> Irc String String
randomTumble query = do
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
      Error s -> textAndUrl ("Oops!" ++ s) url
  where url = "http://" ++ query ++ ".tumblr.com/api/read/json?num=50"

tumblr :: String -> Irc String String
tumblr query = case words query of
                "last":_ -> lastTumble $ dropWhile isSpace . drop 4 $ query 
                _ -> randomTumble query

handler :: Handler String
handler = makeBangHandler "tumblr" ["!tumblr"] tumblr

