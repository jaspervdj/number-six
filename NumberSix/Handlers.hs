{-# LANGUAGE Rank2Types #-}
module NumberSix.Handlers
    ( handlers
    ) where

import NumberSix.Irc (SomeHandler (..))

import qualified NumberSix.Handlers.AddGod
import qualified NumberSix.Handlers.Binary
import qualified NumberSix.Handlers.Cubits
import qualified NumberSix.Handlers.Down
import qualified NumberSix.Handlers.EightBall
import qualified NumberSix.Handlers.GitHub
import qualified NumberSix.Handlers.Gods
import qualified NumberSix.Handlers.Google
import qualified NumberSix.Handlers.HackerNews
import qualified NumberSix.Handlers.Hello
import qualified NumberSix.Handlers.Help
import qualified NumberSix.Handlers.Identify
import qualified NumberSix.Handlers.Kick
import qualified NumberSix.Handlers.LastFm
import qualified NumberSix.Handlers.NowPlaying
import qualified NumberSix.Handlers.Op
import qualified NumberSix.Handlers.Ping
import qualified NumberSix.Handlers.Quote
import qualified NumberSix.Handlers.Seen
import qualified NumberSix.Handlers.Shorten
import qualified NumberSix.Handlers.Slap
import qualified NumberSix.Handlers.Tell
import qualified NumberSix.Handlers.Title
import qualified NumberSix.Handlers.TryHaskell
import qualified NumberSix.Handlers.Twitter
import qualified NumberSix.Handlers.UrbanDictionary

handlers :: [SomeHandler]
handlers =
    [ SomeHandler NumberSix.Handlers.AddGod.handler
    , SomeHandler NumberSix.Handlers.Binary.handler
    , SomeHandler NumberSix.Handlers.Cubits.handler
    , SomeHandler NumberSix.Handlers.Down.handler
    , SomeHandler NumberSix.Handlers.EightBall.handler
    , SomeHandler NumberSix.Handlers.GitHub.handler
    , SomeHandler NumberSix.Handlers.Gods.handler
    , SomeHandler NumberSix.Handlers.Google.handler
    , SomeHandler NumberSix.Handlers.HackerNews.handler
    , SomeHandler NumberSix.Handlers.Hello.handler
    , SomeHandler NumberSix.Handlers.Help.handler
    , SomeHandler NumberSix.Handlers.Identify.handler
    , SomeHandler NumberSix.Handlers.Kick.handler
    , SomeHandler NumberSix.Handlers.LastFm.handler
    , SomeHandler NumberSix.Handlers.NowPlaying.handler
    , SomeHandler NumberSix.Handlers.Op.handler
    , SomeHandler NumberSix.Handlers.Ping.handler
    , SomeHandler NumberSix.Handlers.Quote.handler
    , SomeHandler NumberSix.Handlers.Seen.handler
    , SomeHandler NumberSix.Handlers.Shorten.handler
    , SomeHandler NumberSix.Handlers.Slap.handler
    , SomeHandler NumberSix.Handlers.Tell.handler
    , SomeHandler NumberSix.Handlers.Title.handler
    , SomeHandler NumberSix.Handlers.TryHaskell.handler
    , SomeHandler NumberSix.Handlers.Twitter.handler
    , SomeHandler NumberSix.Handlers.UrbanDictionary.handler
    ]
