--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Imdb.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework          (Test, testGroup)
import           Test.HUnit              ((@=?))


--------------------------------------------------------------------------------
import           NumberSix.Handlers.Imdb
import           NumberSix.Message
import           NumberSix.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Handlers.Imdb.Tests"
    [ cases "imdb"
        [ do
            result <- imdb "once upon a time in america"
            result @=? "Once Upon a Time in America (1984): 8.4 " <>
                ">> http://www.imdb.com/title/tt0087843/"
        ]
    ]
