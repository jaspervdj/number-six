module NumberSix.Handlers.Hello
    ( handler
    ) where

import NumberSix.Irc

handler :: Handler
handler = makeHandler "hello" $
    onBangCommand "!hello" $ writeChannelReply "O hai!"
