-- | Checks if a website is down
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Down
    ( handler
    ) where

import Control.Exception (try, SomeException (..))
import Control.Monad.Trans (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Network.Curl

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util.Http

down :: ByteString -> Irc ByteString
down query = do
    result <- liftIO $ try $
        curlGetResponse_ (B.unpack $ httpPrefix query) curlOptions
    return $ query <> case result of
        -- Catch all ugly stuff
        Left (SomeException _) -> " looks down from Caprica."
        -- Got a response
        Right response -> case respCurlCode (fixType' response) of
            CurlOK -> " seems to be working fine, stop whining."
            _ -> " gave a nasty " <> (B.pack $ show $ respStatus response)
                                  <> "."
  where
    -- Fix the ambigious types
    fixType' :: CurlResponse_ [(String, String)] ByteString ->
                CurlResponse_ [(String, String)] ByteString
    fixType' = id

handler :: UninitializedHandler
handler = makeBangHandler "down" ["!down"] down
