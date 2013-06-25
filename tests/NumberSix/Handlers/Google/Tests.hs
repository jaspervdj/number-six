--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Google.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework            (Test, testGroup)
import           Test.HUnit                ((@=?))


--------------------------------------------------------------------------------
import           NumberSix.Handlers.Google
import           NumberSix.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Handlers.Google.Tests"
    [ cases "google"
        [ do
            result <- google "jaspervdj"
            result @=? "jaspervdj - Home >> http://jaspervdj.be/"
        ]
    ]
