-- | Get flickr posts for a given user
--
module NumberSix.Handlers.Flickr
    ( handler
    ) where

import Control.Monad
import qualified Control.Monad.Trans as CMT
import Data.Maybe(fromJust)
import System.Random (randomRIO)

import Flickr.Monad
import Flickr.People
import Flickr.Photos
import Flickr.Types
import Flickr.URLs
import Util.Keys

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.BitLy
import NumberSix.Util.Http

type URL = String

-- | get the public photos for a given user by the user's URL
--
publicPhotos :: URL -> FM [Photo]
publicPhotos userURL = do
    u <- tryFlick $ lookupUser userURL 
    case u of
      Left err -> return []
      Right user -> getPublicPhotos (userId user) Nothing []

-- | Actual handler implementation.
-- XXX: Needs cleanup and more functionality
flickr :: String -> Irc String String
flickr query = do
    let user : _ = words query
        url = buildUserURL user
    photoURLs <- CMT.liftIO $ flickAPI hsflickrAPIKey $ publicPhotos url >>= \ps -> mapM (\p -> liftM photoDetailsURLs $ Flickr.Photos.getInfo (photoId p) Nothing) ps
    report ("Hey ... we're still living ... looking up for " ++ user)
    case photoURLs of
      (us:_) -> let us' = filter (\u -> urlDetailsType u == "photopage") us
                in case us' of 
                      (u:_) -> textAndUrl ("Last photo of " ++ user ++ ": ") (urlDetailsURL u)
                      _ -> do report "OOPS"
                              textAndUrl ("Cannot obtain URL for the last photo of " ++ user) ""
      [] -> do report ("Did not find any photos for " ++ user)
               textAndUrl ("Cannot obtain URL for the last photo of " ++ user) ""

  where buildUserURL u = "http://flickr.com/photos/" ++ u


handler :: Handler String
handler = makeBangHandler "flickr" ["!flickr"] flickr
