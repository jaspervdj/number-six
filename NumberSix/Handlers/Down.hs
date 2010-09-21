-- | Checks if a website is down
--
module NumberSix.Handlers.Down
    ( handler
    ) where

import Control.Exception (try, SomeException (..))
import Control.Monad.Trans (liftIO)

import Network.Curl

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

down :: String -> Irc String String
down query = do
    result <- liftIO $ try $
        curlGetResponse_ (httpPrefix query) curlOptions
    return $ query <> case result of
        -- Catch all ugly stuff
        Left (SomeException _) -> " looks down from Caprica."
        -- Got a response
        Right response -> case respCurlCode (fixType' response) of
            CurlOK -> " seems to be working fine, stop whining."
            _ -> " gave a nasty " <> (show $ respStatus response)
                                  <> "."
  where
    -- Fix the ambigious types
    fixType' :: CurlResponse_ [(String, String)] String ->
                CurlResponse_ [(String, String)] String
    fixType' = id

handler :: Handler String
handler = makeBangHandler "down" ["!down"] down
