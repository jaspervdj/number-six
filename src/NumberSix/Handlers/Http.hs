--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Http
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Exception     (SomeException(..), catch)
import           Control.Monad.Trans   (liftIO)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as H
import qualified Network.HTTP.Types    as H
import           Prelude               hiding (catch)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.Http   (httpPrefix)


--------------------------------------------------------------------------------
http :: ByteString -> IO ByteString
http uri = do
    req <- H.parseUrl uri'
    let req' = req {H.redirectCount = 0, H.checkStatus = \_ _ _ -> Nothing}
    rsp <- H.withManager $ \m -> H.httpLbs req' m
    let status   = H.responseStatus rsp
        location
            | H.statusCode status < 300  = ""
            | H.statusCode status >= 400 = ""
            | otherwise                  =
                case lookup "Location" (H.responseHeaders rsp) of
                    Nothing  -> ""
                    Just loc -> " (Location: " <> loc <> ")"

    return $ BC.pack (show $ H.responseVersion rsp) <> " " <>
        BC.pack (show $ H.statusCode status) <> " " <>
        H.statusMessage status <> location
  where
    uri' = T.unpack $ T.decodeUtf8 $ httpPrefix uri


--------------------------------------------------------------------------------
-- | Catch possible network errors
wrapped :: ByteString -> IO ByteString
wrapped uri = catch (http uri) $ \(SomeException e) ->
    return $ T.encodeUtf8 $ T.pack $ show e


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Http" ["!http"] $ liftIO . wrapped
