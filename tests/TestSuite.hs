--------------------------------------------------------------------------------
import Test.Framework (defaultMain)


--------------------------------------------------------------------------------
import qualified NumberSix.Message.Decode.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ NumberSix.Message.Decode.Tests.tests
    ]
