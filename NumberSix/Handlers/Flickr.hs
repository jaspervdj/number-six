-- | Get flickr posts for a given user
--
module NumberSix.Handlers.Flickr
    ( handler
    ) where

import Control.Monad
import qualified Control.Monad.Trans as CMT
import Data.Maybe(fromJust)
import System.Random (randomRIO)

import Flickr.Favorites
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
publicPhotos :: URL -> Maybe Int -> FM [Photo]
publicPhotos userURL limit = do
    u <- tryFlick $ lookupUser userURL 
    case u of
      Left err -> return []
      Right user -> case limit of
                      Nothing -> id
                      Just n -> withPageSize n
                    $ getPublicPhotos (userId user) Nothing []

-- | Get the last public available favourite for the given user
--
publicFaves :: URL -> FM [Photo]
publicFaves userURL = do
    u <- tryFlick $ lookupUser userURL
    case u of
      Left err -> return []
      Right user -> withPageSize 1 $ liftM snd $ getPublicList (userId user) [] nullDateDetails


-- | Actual handler implementation.
-- XXX: Needs cleanup and more functionality
flickr :: String -> Irc String String
flickr query = 
    case words query of
      command : user : _ -> do
        let url = buildUserURL user
        let photos = case command of
                    "last" -> publicPhotos url (Just 1)
                    "random" -> do ps <- publicPhotos url Nothing
                                   r <- liftIO $ randomRIO (1, length ps)
                                   return (ps !! r : [])
                    "fave" -> publicFaves url

        photoURLs <- CMT.liftIO $ flickAPI hsflickrAPIKey $ photos >>= \ps-> mapM (\p -> liftM photoDetailsURLs $ Flickr.Photos.getInfo (photoId p) Nothing) ps
        -- We make sure we only have one.
        case photoURLs of
          (us:_) -> let us' = filter (\u -> urlDetailsType u == "photopage") us
                        in case us' of 
                             (u:_) -> textAndUrl (command ++ " photo of " ++ user ++ ": ") (urlDetailsURL u)
                             _ -> return ("Cannot obtain URL for the last photo of " ++ user)
          [] -> return ("Cannot obtain URL for the last photo of " ++ user) 

      _ -> return ("Usage: !flickr <command> <user> where command is one of: last, random, fave")
  where buildUserURL u = "http://flickr.com/photos/" ++ u


handler :: Handler String
handler = makeBangHandler "flickr" ["!flickr"] flickr
