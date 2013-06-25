--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Imdb.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString         as B
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
            assert $ "Once Upon a Time in America (1984)" `B.isPrefixOf` bs
            assert $ "http://www.imdb.com/title/tt0087843/" `B.isSuffixOf` bs
        ]
    ]
