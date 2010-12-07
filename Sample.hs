-- | Number six sample configuration file
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import NumberSix
import NumberSix.Irc

main :: IO ()
main = numberSix IrcConfig
    { ircNick        = "mempty"
    , ircRealName    = "Number Six Testing Bot"
    , ircChannels    = ["#testing"]
    , ircHost        = "wina.ugent.be"
    , ircPort        = 6666
    , ircGodPassword = "foobar"
    }
