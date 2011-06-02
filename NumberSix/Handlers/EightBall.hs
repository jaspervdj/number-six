-- | Simple 8ball handler
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.EightBall
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util

handler :: Handler
handler = makeHandler "eightball" $ return $ onBangCommand "!8ball" $
    randomElement answers >>= writeReply
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
