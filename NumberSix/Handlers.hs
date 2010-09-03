module NumberSix.Handlers
    ( handlers
    ) where

import NumberSix.Irc (Irc, Handler)

import qualified NumberSix.Handlers.Ping
import qualified NumberSix.Handlers.Identify
import qualified NumberSix.Handlers.Hello
import qualified NumberSix.Handlers.Google
import qualified NumberSix.Handlers.Kick
import qualified NumberSix.Handlers.Twitter
import qualified NumberSix.Handlers.Tell
import qualified NumberSix.Handlers.Seen

handlers :: [Handler]
handlers =
    [ NumberSix.Handlers.Ping.handler
    , NumberSix.Handlers.Identify.handler
    , NumberSix.Handlers.Hello.handler
    , NumberSix.Handlers.Google.handler
    , NumberSix.Handlers.Kick.handler
    , NumberSix.Handlers.Twitter.handler
    , NumberSix.Handlers.Tell.handler
    , NumberSix.Handlers.Seen.handler
    ]
