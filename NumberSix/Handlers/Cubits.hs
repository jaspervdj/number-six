module NumberSix.Handlers.Cubits
    ( handler
    ) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Maybe (fromMaybe)

import NumberSix.Irc
import NumberSix.IrcString
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Redis

handler :: Handler String
handler = makeHandler "cubits" [cubitsHook]

cubitsHook :: Irc String ()
cubitsHook = onBangCommand "!cubits" $ do
    args <- words <$> getBangCommandText
    sender <- getSender
    case args of
        [] -> withCubits id sender
        (nick : []) -> withCubits id nick
        (nick : n' : []) -> onGod $ unless (nick ==? sender) $ do
            let n = read n'
                sn = show $ abs n
            write $ meAction $ if n >= 0
                then "gives " <> nick <> " " <> sn <> " cubits."
                else "takes " <> sn <> " cubits from " <> nick <> "."
            withCubits (+ n) nick
        _ -> return ()

withCubits :: (Integer -> Integer) -> String -> Irc String ()
withCubits f nick = withRedis $ \redis -> do
    cubits <- f . fromMaybe 0 <$> getItem redis ChannelRealm nick
    setItem redis ChannelRealm nick cubits
    write $ nick <> " has " <> (show cubits) <> " cubits."
