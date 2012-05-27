{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Shorten
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans  (liftIO)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util.BitLy


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Shorten" ["!shorten"] $ liftIO . shorten
