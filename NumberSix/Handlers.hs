module NumberSix.Handlers
    ( handlers
    ) where

import NumberSix.Irc (Handler)

import qualified NumberSix.Handlers.Ping
import qualified NumberSix.Handlers.Identify
import qualified NumberSix.Handlers.Hello
import qualified NumberSix.Handlers.Google
import qualified NumberSix.Handlers.Kick
import qualified NumberSix.Handlers.Twitter
import qualified NumberSix.Handlers.Tell
import qualified NumberSix.Handlers.Seen
import qualified NumberSix.Handlers.Binary
import qualified NumberSix.Handlers.TryHaskell
import qualified NumberSix.Handlers.Quote
import qualified NumberSix.Handlers.EightBall
import qualified NumberSix.Handlers.UrbanDictionary
import qualified NumberSix.Handlers.LastFm
import qualified NumberSix.Handlers.GitHub
import qualified NumberSix.Handlers.Down
import qualified NumberSix.Handlers.Title
import qualified NumberSix.Handlers.HackerNews
import qualified NumberSix.Handlers.Shorten
import qualified NumberSix.Handlers.Slap
import qualified NumberSix.Handlers.Gods
import qualified NumberSix.Handlers.AddGod

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
    , NumberSix.Handlers.Binary.handler
    , NumberSix.Handlers.TryHaskell.handler
    , NumberSix.Handlers.Quote.handler
    , NumberSix.Handlers.EightBall.handler
    , NumberSix.Handlers.UrbanDictionary.handler
    , NumberSix.Handlers.LastFm.handler
    , NumberSix.Handlers.GitHub.handler
    , NumberSix.Handlers.Down.handler
    , NumberSix.Handlers.Title.handler
    , NumberSix.Handlers.HackerNews.handler
    , NumberSix.Handlers.Shorten.handler
    , NumberSix.Handlers.Slap.handler
    , NumberSix.Handlers.Gods.handler
    , NumberSix.Handlers.AddGod.handler
    ]
