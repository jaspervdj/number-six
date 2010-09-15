module NumberSix.Handlers.Cubits
    ( handler
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Redis

handler :: Handler
handler = makeHandler "cubits" cubitsHook

cubitsHook :: Irc ()
cubitsHook = onBangCommand "!cubits" $ do
    args <- words <$> getBangCommandText
    case args of
        [] -> getSender >>= withCubits id
        [nick] -> withCubits id nick
        [nick, n'] -> onGod $ do
            let n = read n'
            writeChannel $ meAction $ if n >= 0
                then "gives " ++ nick ++ " " ++ show n ++ " cubits."
                else "takes " ++ show (- n) ++ " cubits from " ++ nick ++ "."
            withCubits (+ n) nick

withCubits :: (Int -> Int) -> String -> Irc ()
withCubits f nick = withRedis $ \redis -> do
    cubits <- f . fromMaybe 0 <$> getItem redis nick
    setItem redis nick cubits
    writeChannel $ nick ++ " has " ++ show cubits ++ " cubits."
