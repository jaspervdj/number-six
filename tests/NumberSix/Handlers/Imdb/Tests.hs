--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Imdb.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text               as T
import           Test.Framework          (Test, testGroup)
import           Test.HUnit              (assert)


--------------------------------------------------------------------------------
import           NumberSix.Handlers.Imdb
import           NumberSix.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Handlers.Imdb.Tests"
    [ cases "imdb"
        [ do
            bs <- imdb "once upon a time in america"
            assert $ "Once Upon a Time in America (1984)" `T.isPrefixOf` bs
            assert $ "http://www.imdb.com/title/tt0087843/" `T.isSuffixOf` bs
        ]
    ]
