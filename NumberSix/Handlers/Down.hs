-- | Checks if a website is down
--
module NumberSix.Handlers.Down
    ( handler
    ) where

import Control.Monad.Trans (liftIO)

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import NumberSix.Irc
import NumberSix.Util.Http

down :: String -> Irc String
down query = do
    let get = getResponseBody =<< simpleHTTP (getRequest $ httpPrefix query)
    result <- liftIO $ catch (fmap (Just . take 100) get)
                             (const $ return Nothing)
    return $ case result of
        -- Catch all exceptions
        Nothing -> query ++ " looks down from Caprica."
        -- All is fine
        Just _ -> query ++ " seems to be working fine, stop whining."

handler :: Handler
handler = makeBangHandler "down" ["!down"] down
