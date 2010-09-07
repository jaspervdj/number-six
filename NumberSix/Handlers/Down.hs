-- | Checks if a website is down
--
module NumberSix.Handlers.Down
    ( handler
    ) where

import Control.Exception (try, SomeException (..))
import Control.Monad.Trans (liftIO)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import NumberSix.Irc

down :: String -> Irc String
down query = do
    let get = getResponseBody =<< simpleHTTP (getRequest query)
    result <- liftIO $ try $ fmap (take 100) get
    return $ case result of
        -- Catch all exceptions
        Left (SomeException _)  -> query ++ " looks down from Caprica."
        -- All is fine
        Right _ -> query ++ " seems to be working fine, stop whining."

handler :: Handler
handler = makeBangHandler "down" ["!down"] down
