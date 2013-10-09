-- | Evaluates Haskell expressions using the TryHaskell API
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.TryHaskell
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<*>), (<|>))
import           Control.Monad         (mzero)
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (FromJSON, Value (..), parseJSON, (.:))
import           Data.Text       (Text)
import qualified Data.Text as T


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Message
import           NumberSix.Util
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data Result
    = Result Text Text  -- Type, value
    | Error Text              -- Error message


--------------------------------------------------------------------------------
instance FromJSON Result where
    parseJSON (Object o) =
        (Result <$> o .: "type" <*> o .: "result") <|>
        (Error . removeNewlines <$> o .: "error")
    parseJSON _          = mzero


--------------------------------------------------------------------------------
eval :: Text -> IO Text
eval query = http url id >>= \bs -> return $ case parseJsonEither bs of
    Left _             -> "Request failed!"
    Right (Result t r) -> if ":t" `T.isPrefixOf` query then t else r
    Right (Error e)    -> "Error: " <> e
  where
    url = "http://tryhaskell.org/haskell.json?method=eval&expr=" <>
        urlEncode query


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "TryHaskell" [">", "!haskell"] $ liftIO . eval
