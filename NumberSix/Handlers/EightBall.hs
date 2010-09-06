-- | Simple 8ball handler
--
module NumberSix.Handlers.EightBall
    ( handler
    ) where

import Control.Monad.Trans (liftIO)
import System.Random (randomRIO)

import NumberSix.Irc

handler :: Handler
handler = makeHandler "eightball" $ onBangCommand "!8ball" $ do
    r <- liftIO $ randomRIO (0, length answers - 1)
    writeChannelReply $ answers !! r
  where
    answers = [ "As I see it, yes"
              , "It is certain"
              , "It is decidedly so"
              , "Most likely"
              , "Outlook good"
              , "Signs point to yes"
              , "Without a doubt"
              , "Yes"
              , "Yes - definitely"
              , "You may rely on it"
              , "Reply hazy, try again"
              , "Ask again later"
              , "Better not tell you now"
              , "Cannot predict now"
              , "Concentrate and ask again"
              , "Don't count on it"
              , "My reply is no"
              , "My sources say no"
              , "Outlook not so good"
              , "Very doubtful"
              ]
