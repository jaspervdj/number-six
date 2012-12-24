--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.TryRuby
    ( ruby
    , handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>))
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON, Value(..), parseJSON, (.:))
import           Data.ByteString      (ByteString)
import qualified Network.HTTP.Conduit as HC


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data Result
    = Success ByteString
    | Error ByteString
    deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Result where
    parseJSON (Object o) = do
        success <- o .: "success"
        if success then Success <$> o .: "output" else Error <$> o .: "result"
    parseJSON _          = mzero


--------------------------------------------------------------------------------
ruby :: ByteString -> IO ByteString
ruby cmd = do
    bs <- http "http://tryruby.org/levels/1/challenges/0" (setPut . setCmd)
    case parseJsonEither bs of
        Right (Success x) -> return x
        Right (Error x)   -> return x
        Left _            -> randomError
  where
    setCmd :: Monad m => HC.Request m -> HC.Request m
    setCmd = HC.urlEncodedBody [("cmd", cmd)]
    setPut rq = rq {HC.method = "PUT"}


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "TryRuby" ["@","!ruby"] $ liftIO . ruby
