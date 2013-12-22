--------------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}
module NumberSix.Handlers
    ( handlers
    ) where


--------------------------------------------------------------------------------
import qualified NumberSix.Handlers.AlfaPapaTango
import qualified NumberSix.Handlers.Binary
import qualified NumberSix.Handlers.Bomb
import qualified NumberSix.Handlers.EightBall
import qualified NumberSix.Handlers.Feed
import qualified NumberSix.Handlers.GitHub
import qualified NumberSix.Handlers.Gods
import qualified NumberSix.Handlers.Google
import qualified NumberSix.Handlers.HackerNews
import qualified NumberSix.Handlers.Hello
import qualified NumberSix.Handlers.Help
import qualified NumberSix.Handlers.Http
import qualified NumberSix.Handlers.Identify
import qualified NumberSix.Handlers.Imdb
import qualified NumberSix.Handlers.Insult
import qualified NumberSix.Handlers.Join
import qualified NumberSix.Handlers.LastFm
import qualified NumberSix.Handlers.NickServ
import qualified NumberSix.Handlers.NowPlaying
import qualified NumberSix.Handlers.Op
import qualified NumberSix.Handlers.Pick
import qualified NumberSix.Handlers.Ping
import qualified NumberSix.Handlers.Quote
import qualified NumberSix.Handlers.Reddit
import qualified NumberSix.Handlers.Remind
import qualified NumberSix.Handlers.Resto
import qualified NumberSix.Handlers.Rr
import qualified NumberSix.Handlers.Say
import qualified NumberSix.Handlers.Seen
import qualified NumberSix.Handlers.Shorten
import qualified NumberSix.Handlers.Sup
import qualified NumberSix.Handlers.Tell
import qualified NumberSix.Handlers.Topic
import qualified NumberSix.Handlers.TryHaskell
import qualified NumberSix.Handlers.TryRuby
import qualified NumberSix.Handlers.Tumblr
import qualified NumberSix.Handlers.Twitter
import qualified NumberSix.Handlers.UrbanDictionary
import qualified NumberSix.Handlers.Voice
import qualified NumberSix.Handlers.Weather
import qualified NumberSix.Handlers.Wikipedia
import qualified NumberSix.Handlers.YouTube
import           NumberSix.Irc


--------------------------------------------------------------------------------
handlers :: [UninitializedHandler]
handlers =
    [ NumberSix.Handlers.AlfaPapaTango.handler
    , NumberSix.Handlers.Binary.handler
    , NumberSix.Handlers.Bomb.handler
    , NumberSix.Handlers.EightBall.handler
    , NumberSix.Handlers.Feed.handler
    , NumberSix.Handlers.GitHub.handler
    , NumberSix.Handlers.Gods.handler
    , NumberSix.Handlers.Google.handler
    , NumberSix.Handlers.HackerNews.handler
    , NumberSix.Handlers.Hello.handler
    , NumberSix.Handlers.Help.handler
    , NumberSix.Handlers.Http.handler
    , NumberSix.Handlers.Identify.handler
    , NumberSix.Handlers.Imdb.handler
    , NumberSix.Handlers.Insult.handler
    , NumberSix.Handlers.Join.handler
    , NumberSix.Handlers.LastFm.handler
    , NumberSix.Handlers.NickServ.handler
    , NumberSix.Handlers.NowPlaying.handler
    , NumberSix.Handlers.Op.handler
    , NumberSix.Handlers.Pick.handler
    , NumberSix.Handlers.Ping.handler
    , NumberSix.Handlers.Quote.handler
    , NumberSix.Handlers.Reddit.handler
    , NumberSix.Handlers.Remind.handler
    , NumberSix.Handlers.Resto.handler
    , NumberSix.Handlers.Rr.handler
    , NumberSix.Handlers.Say.handler
    , NumberSix.Handlers.Seen.handler
    , NumberSix.Handlers.Shorten.handler
    , NumberSix.Handlers.Sup.handler
    , NumberSix.Handlers.Tell.handler
    , NumberSix.Handlers.Topic.handler
    , NumberSix.Handlers.TryHaskell.handler
    , NumberSix.Handlers.TryRuby.handler
    , NumberSix.Handlers.Tumblr.handler
    , NumberSix.Handlers.Twitter.handler
    , NumberSix.Handlers.UrbanDictionary.handler
    , NumberSix.Handlers.Voice.handler
    , NumberSix.Handlers.Weather.handler
    , NumberSix.Handlers.Wikipedia.handler
    , NumberSix.Handlers.YouTube.handler
    ]
