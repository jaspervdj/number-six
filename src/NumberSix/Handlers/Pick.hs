-- | Pick a random item from a list
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Pick
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import qualified Data.Text           as T


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Pick" ["!pick", "!who"] $
    liftIO . randomElement . T.words
