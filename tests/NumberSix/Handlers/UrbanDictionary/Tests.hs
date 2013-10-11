--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.UrbanDictionary.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text                          as T
import           Test.Framework                     (Test, testGroup)
import           Test.HUnit                         (assert)


--------------------------------------------------------------------------------
import           NumberSix.Handlers.UrbanDictionary
import           NumberSix.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Handlers.UrbanDictionary.Tests"
    [ cases "urban"
        [ do
            rsp <- urban "yolo"
            assert $ "you only live once" `T.isInfixOf` T.toLower rsp
        ]
    ]
