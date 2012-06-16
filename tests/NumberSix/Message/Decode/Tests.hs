--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Message.Decode.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework           (Test, testGroup)
import           Test.HUnit               ((@=?))


--------------------------------------------------------------------------------
import           NumberSix.Message
import           NumberSix.Message.Decode
import           NumberSix.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Message.Decode.Tests" 
    [ decodeTests
    ]


--------------------------------------------------------------------------------
decodeTests :: Test
decodeTests = cases "decode"
    [ Message n "QUIT" [] @=??
        decode "QUIT"

    , Message (j (NickPrefix "nudded" n n)) "QUIT" [] @=??
        decode ":nudded QUIT"

    , Message
        (j (NickPrefix "Itkovian" (j "Itkovian") (j "assail.ugent.be")))
        "PRIVMSG"
        ["#zeus", "lol wut"] @=??
        decode ":Itkovian!Itkovian@assail.ugent.be PRIVMSG #zeus :lol wut"

    , Message
        (j (NickPrefix "relix" n (j "irccloud.com")))
        "JOIN"
        ["#zeus"] @=??
        decode ":relix@irccloud.com JOIN #zeus"

    , Message
        (j (ServerPrefix "wina.ugent.be"))
        "001"
        ["number-six", "Welcome to the Kelder Network"] @=??
        decode ":wina.ugent.be 001 number-six :Welcome to the Kelder Network"
    ]
  where
    -- Convenience
    n        = Nothing
    j        = Just
    x @=?? y = Just x @=? y
