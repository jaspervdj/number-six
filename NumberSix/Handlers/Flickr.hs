-- | Get flickr posts for a given user
--
module NumberSix.Handlers.Flickr
    ( handler
    ) where

import Data.List (find)
import Control.Applicative ((<$>))
import Control.Monad
import qualified Control.Monad.Trans as CMT
import Data.Maybe (fromMaybe, listToMaybe)
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
import NumberSix.Util.BitLy

type URL = String

-- | get the public photos for a given user by the user's URL
--
publicPhotos :: URL -> Maybe Int -> FM [Photo]
publicPhotos userURL limit = do
    u <- tryFlick $ lookupUser userURL 
    case u of
        Left _ -> return []
        Right user -> fromMaybe id (withPageSize <$> limit) $
            getPublicPhotos (userId user) Nothing []

-- | Get the last public available favourite for the given user
--
publicFaves :: URL -> FM [Photo]
publicFaves userURL = do
    u <- tryFlick $ lookupUser userURL
    case u of
        Left _ -> return []
        Right user -> withPageSize 1 $ liftM snd $
            getPublicList (userId user) [] nullDateDetails

-- | Actual handler implementation.
--
-- XXX: Needs cleanup and more functionality
--
flickr :: String -> Irc String String
flickr query = case words query of
    -- Succesfully parsed command
    command : user : _ -> do
        -- URL search 
        photoURLs <- CMT.liftIO $ flickAPI hsflickrAPIKey $ do
            -- Search photos. Try to limit returned list, if possible
            let url = "http://flickr.com/photos/" ++ url
            photos <- case command of
                          "last" -> publicPhotos url (Just 1)
                          "random" -> do ps <- publicPhotos url Nothing
                                         r <- liftIO $ randomRIO (1, length ps)
                                         return (ps !! r : [])
                          "fave" -> publicFaves url
                          _      -> return []

            -- Get details (URL) for every photo found
            forM photos $ \p -> liftM photoDetailsURLs $
                Flickr.Photos.getInfo (photoId p) Nothing

        -- Obtain the URL details
        let url = do
                    us <- listToMaybe photoURLs
                    us' <- find ((== "photopage") . urlDetailsType) us
                    return $ urlDetailsURL us'

        -- Finnish!
        case url of
            Nothing -> return "Not found"
            Just u -> textAndUrl (command ++ " photo of " ++ user) u
  
    -- Parse command failed
    _ -> return "Usage: !flickr <command> <user> where command is one of: \
                \last, random, fave"

handler :: Handler String
handler = makeBangHandler "flickr" ["!flickr"] flickr
