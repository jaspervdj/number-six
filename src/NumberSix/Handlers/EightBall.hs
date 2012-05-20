-- | Simple 8ball handler
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.EightBall
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc


--------------------------------------------------------------------------------
hashMod :: Int -> ByteString -> Int
hashMod max' bs = if hash < 0 then hash + max' else hash
  where
    hash = B.foldl' step 5381 bs `mod` max'
    step x c = x * 33 + fromIntegral c


--------------------------------------------------------------------------------
eightball :: ByteString -> ByteString
eightball = (answers !!) . hashMod (length answers)
  where
    answers =
        [ "As I see it, yes"
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


--------------------------------------------------------------------------------
handler :: UninitiazedHandler
handler = makeBangHandler "eightball" ["!8ball"] $ return . eightball
