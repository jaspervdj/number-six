--------------------------------------------------------------------------------
-- Testing Utilities
module NumberSix.Tests.Util
    ( cases
    ) where


--------------------------------------------------------------------------------
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)


--------------------------------------------------------------------------------
cases :: String -> [Assertion] -> Test
cases name assertions = testGroup name
    [ testCase ("case " ++ show i) assertion
    | (i, assertion) <- zip [1 :: Int ..] assertions
    ]
