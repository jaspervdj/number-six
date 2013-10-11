--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Twitter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework             (Test, testGroup)
import           Test.HUnit                 ((@?=))


--------------------------------------------------------------------------------
import           NumberSix.Handlers.Twitter
import           NumberSix.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Handlers.Twitter.Tests"
    [ cases "twitter"
        [ do
            rsp <- twitter "252913352601399296"
            rsp @?= "@jaspervdj: Japan has really fluffy mountains"
        ]
    ]
