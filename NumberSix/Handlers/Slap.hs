-- | Handler to express superiority
--
module NumberSix.Handlers.Slap
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = makeBangHandler "slap" ["!slap"] $ \nick -> do
    myNick <- getNick
    sender <- getSender
    gods <- getGods
    let bitch = if nick == myNick || nick `elem` gods then sender else nick
    return $ meAction $
        "slaps " ++ bitch ++ " around a bit with a large trout, out of sheer "
                 ++ "superiority."
