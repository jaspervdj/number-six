-- | Get flickr posts for a given user
--
module NumberSix.Handlers.Flickr
    ( handler
    ) where

import Control.Monad
import qualified Control.Monad.Trans as CMT
import Data.Maybe(fromJust)

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
publicPhotos :: URL -> FM  (Maybe [Photo])
publicPhotos userURL = do
    u <- tryFlick $ lookupUser userURL 
    case u of
      Left err -> return Nothing
      Right user -> liftM Just $ getPublicPhotos (userId user) Nothing []

-- | Actual handler implementation.
-- XXX: Needs cleanup and more functionality
flickr :: String -> Irc String String
flickr query = do
    let user : _ = words query
        url = buildUserURL user
    photos <- CMT.liftIO $ flickAPI hsflickrAPIKey $ publicPhotos url -- >>= \ps -> mapM (\p -> liftM photoDetailsURLs $ Flickr.Photos.getInfo (photoId p) Nothing) ps
    case photos of
      Nothing -> do report ("Did not find any photos for " ++ user) 
                    textAndUrl ("Something went wrong when looking up the photos of " ++ user) url
      Just [] -> textAndUrl ("No photos available for " ++ user) ""
      Just ps -> do report ("Found " ++ (show $ length ps) ++ " photos for " ++ user)
                    let p = head ps 
                    photoUrl <- CMT.liftIO . flickAPI hsflickrAPIKey . liftM photoDetailsURLs $ Flickr.Photos.getInfo (photoId p) Nothing
                    case photoUrl of
                      [] -> textAndUrl ("Cannot obtain URL for the last photo of " ++ user) ""
                      us -> let us' = filter (\u -> urlDetailsType u == "photopage") us
                            in case us' of
                              [] -> textAndUrl ("Cannot obtain URL for the last photo of " ++ user) ""
                              (u:_) -> textAndUrl ("Last photo of " ++ user ++ ": ") (urlDetailsURL u)
      _ -> do textAndUrl ("Flickr plugin: this should not happen") ""
              
  where buildUserURL u = "http://flickr.com/photos/" ++ u


handler :: Handler String
handler = makeBangHandler "flickr" ["!flickr"] flickr
