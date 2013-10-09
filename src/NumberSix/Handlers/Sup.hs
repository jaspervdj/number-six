--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Sup
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (when)
import qualified Data.Text           as T


--------------------------------------------------------------------------------
import           NumberSix.Irc
import           NumberSix.Message


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandler "Sup" [supHook]


--------------------------------------------------------------------------------
supHook :: Irc ()
supHook = onCommand "PRIVMSG" $ do
    text <- getMessageText
    expected <- ("sup " <>) <$> getNick
    when (expected `T.isPrefixOf` text) $ do
        sender <- getSender
        write $ "sup " <> sender
