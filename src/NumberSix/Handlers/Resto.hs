{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Resto
    ( handler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (mzero)
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (FromJSON (..), Value (..))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy     as HM
import qualified Data.Map              as M
import           Data.Maybe            (maybeToList)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Time             (UTCTime (..), addDays, formatTime,
                                        getCurrentTime)
import qualified Data.Vector           as V
import           System.Locale         (defaultTimeLocale)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util
import           NumberSix.Util.Error
import           NumberSix.Util.Http


--------------------------------------------------------------------------------
data WeekMenu = WeekMenu (M.Map Text [Text]) deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON WeekMenu where
    parseJSON (Object o) = return $ WeekMenu $ M.fromListWith (++)
        [ (day, [T.strip $ stripMenu name])
        | (day, Object menu) <- HM.toList o
        , Array meats        <- maybeToList $ HM.lookup "meat" menu
        , Object meat        <- V.toList meats
        , String name        <- maybeToList $ HM.lookup "name" meat
        ]
    parseJSON _          = mzero


--------------------------------------------------------------------------------
-- | Strip trailing # or * character
stripMenu :: Text -> Text
stripMenu bs
    | "*" `T.isSuffixOf` bs || "#" `T.isSuffixOf` bs = T.init bs
    | otherwise                                      = bs


--------------------------------------------------------------------------------
resto :: ByteString -> IO ByteString
resto arg = do
    currentTime <- getCurrentTime
    let (d, e) = days arg
        time   = currentTime {utctDay = d `addDays` utctDay currentTime}
        week   = formatTime defaultTimeLocale "%V"       time
        day    = formatTime defaultTimeLocale "%Y-%m-%d" time
        url    = "http://zeus.ugent.be/hydra/api/1.0/resto/week/" ++
            dropWhile (== '0') week ++ ".json"

    http (BC.pack url) id >>= \bs -> case parseJsonEither bs of
        Left _             -> randomError
        Right (WeekMenu m) -> return $ case M.lookup (T.pack day) m of
            Nothing -> "Resto's not open " <> e <> "..."
            Just ms -> T.encodeUtf8 $ T.intercalate ", " ms
  where
    days "tomorrow"           = (1, "tomorrow")
    days "morgen"             = (1, "tomorrow")
    days "day after tomorrow" = (2, "the day after tomorrow")
    days "overmorgen"         = (2, "the day after tomorrow")
    days _                    = (0, "today")


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Resto" ["!resto"] (liftIO . resto)
