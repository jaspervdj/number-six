-- | Exposes the datastructure describing an IRC message
module NumberSix.Message
    ( Prefix (..)
    , Message (..)
    , makeMessage
    , (<>)
    , (==?)
    , toLower
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char             as Char (toLower)
import           Data.Monoid           (Monoid, mappend)


--------------------------------------------------------------------------------
data Prefix
    = ServerPrefix ByteString
    | NickPrefix ByteString (Maybe ByteString) (Maybe ByteString)
    deriving (Show, Eq)


--------------------------------------------------------------------------------
data Message = Message
    { messagePrefix     :: Maybe Prefix
    , messageCommand    :: ByteString
    , messageParameters :: [ByteString]
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
makeMessage :: ByteString -> [ByteString] -> Message
makeMessage = Message Nothing


--------------------------------------------------------------------------------
(<>) :: Monoid m => m -> m -> m
(<>) = mappend


--------------------------------------------------------------------------------
-- | Case-insensitive comparison
(==?) :: ByteString -> ByteString -> Bool
s1 ==? s2 = toLower s1 == toLower s2


--------------------------------------------------------------------------------
toLower :: ByteString -> ByteString
toLower = B.map toLower'
  where
    -- See IRC RFC
    toLower' '['  = '{'
    toLower' ']'  = '}'
    toLower' '\\' = '|'
    toLower' '~'  = '^'
    toLower' x    = Char.toLower x
