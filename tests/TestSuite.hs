--------------------------------------------------------------------------------
import           Test.Framework                 (defaultMain)


--------------------------------------------------------------------------------
import qualified NumberSix.Handlers.Imdb.Tests
import qualified NumberSix.Message.Decode.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ NumberSix.Handlers.Imdb.Tests.tests
    , NumberSix.Message.Decode.Tests.tests
    ]
