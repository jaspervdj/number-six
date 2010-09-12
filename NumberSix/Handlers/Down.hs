-- | Checks if a website is down
--
module NumberSix.Handlers.Down
    ( handler
    ) where

import Control.Exception (try, SomeException (..))
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT, ask)

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.Http

down :: String -> Irc String
down query = do
    env <- ask
    result <- liftIO $ try $ runReaderT (httpGet SimpleHttp query) env
    return $ case result of
        -- Catch all exceptions
        Left (SomeException _)  -> query ++ " looks down from Caprica."
        -- All is fine
        Right _ -> query ++ " seems to be working fine, stop whining."

handler :: Handler
handler = makeBangHandler "down" ["!down"] down
