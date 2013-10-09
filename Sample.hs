-- | Number six sample configuration file
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import NumberSix
import NumberSix.Irc

main :: IO ()
main = numberSix IrcConfig
    { ircNick        = "herpy"
    , ircRealName    = "Number Six Testing Bot"
    , ircChannels    = ["#testing"]
    , ircHost        = "wina.ugent.be"
    , ircPort        = 6669
    , ircGodPassword = "foobar"
    , ircDatabase    = "number-six.db"
    , ircNickServ    = Just ("NickServ@services.wina.ugent.be", "auth hunter2")
    }
