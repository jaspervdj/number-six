--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Identify
    ( handler
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text     as T


--------------------------------------------------------------------------------
import           NumberSix.Irc


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeHandlerWith "Identify" [] initialize


--------------------------------------------------------------------------------
initialize :: Irc ()
initialize = do
    nick'     <- getNick
    realName' <- getRealName
    writeMessage "NICK" [nick']
    writeMessage "USER" [T.toUpper nick', "*", "*", realName']
