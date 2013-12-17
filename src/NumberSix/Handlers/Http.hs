--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Http
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Exception    (SomeException (..), handle)
import           Control.Monad.Trans  (liftIO)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types   as H


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.Http  (httpPrefix)


--------------------------------------------------------------------------------
http :: Text -> IO Text
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

    return $ T.pack (show $ H.responseVersion rsp) <> " " <>
        T.pack (show $ H.statusCode status) <> " " <>
        T.decodeUtf8 (H.statusMessage status) <> T.decodeUtf8 location
  where
    uri' = T.unpack $ httpPrefix uri


--------------------------------------------------------------------------------
-- | Catch possible network errors
wrapped :: Text -> IO Text
wrapped uri = handle
    (\(SomeException e) -> return $ T.pack $ show e)
    (http uri)


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Http" ["!http"] $ liftIO . wrapped
