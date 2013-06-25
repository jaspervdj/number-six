--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.TryRuby.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework             (Test, testGroup)
import           Test.HUnit                 ((@=?))


--------------------------------------------------------------------------------
import           NumberSix.Handlers.TryRuby
import           NumberSix.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Handlers.TryRuby.Tests"
    [ cases "ruby"
        [ do
            result <- ruby "1 + 1"
            result @=? "=> 2"
        ]
    ]
