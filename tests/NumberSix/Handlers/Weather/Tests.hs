--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Weather.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                 (fromJust)
import           Test.Framework             (Test, testGroup)
import           Test.HUnit                 (assert)


--------------------------------------------------------------------------------
import           NumberSix.Handlers.Weather
import           NumberSix.Tests.Util



--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "NumberSix.Handlers.Weather.Tests"
    [ cases "weather"
        [ do
            result <- getWeather "gent"
            let (Weather (Temperature t) _) = fromJust result
            assert $ -15 <= t && t <= 60
        ]
    ]
