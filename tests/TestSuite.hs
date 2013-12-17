--------------------------------------------------------------------------------
import           Test.Framework                           (defaultMain)


--------------------------------------------------------------------------------
import qualified NumberSix.Handlers.Google.Tests
import qualified NumberSix.Handlers.Imdb.Tests
import qualified NumberSix.Handlers.TryRuby.Tests
import qualified NumberSix.Handlers.Twitter.Tests
import qualified NumberSix.Handlers.UrbanDictionary.Tests
import qualified NumberSix.Handlers.Weather.Tests
import qualified NumberSix.Message.Decode.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ NumberSix.Handlers.Google.Tests.tests
    , NumberSix.Handlers.Imdb.Tests.tests
    , NumberSix.Handlers.TryRuby.Tests.tests
    , NumberSix.Handlers.Twitter.Tests.tests
    , NumberSix.Handlers.UrbanDictionary.Tests.tests
    , NumberSix.Handlers.Weather.Tests.tests
    , NumberSix.Message.Decode.Tests.tests
    ]
