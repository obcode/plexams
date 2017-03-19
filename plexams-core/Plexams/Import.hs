{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import where

import           Control.Applicative  (empty, (<$>), (<*>))
import           Data.Aeson           (FromJSON, Value (Object), decode,
                                       parseJSON, (.:))
import qualified Data.ByteString.Lazy as BS
import           Data.Time            (Day)
import           Data.Time.Format     (defaultTimeLocale, parseTimeM)
import           Plexams.Types

instance FromJSON SemesterConfig where
    parseJSON (Object v) = makeSemesterConfig <$>
                            v .: "semester" <*>
                            v .: "firstDay" <*>
                            v .: "lastDay" <*>
                            v .: "slotsPerDay"
    parseJSON _          = empty

makeSemesterConfig :: String -> String -> String -> [String] -> SemesterConfig
makeSemesterConfig s f l sl = SemesterConfig s (makeDay f) (makeDay l) sl
    where makeDay :: String -> Day
          makeDay str = case parseTimeM True defaultTimeLocale "%d.%m.%Y" str of
                            Nothing  -> error $ "cannot parse date: " ++ str
                            Just day -> day

importSemesterConfigFromJSONFile :: FilePath -> IO (Maybe SemesterConfig)
importSemesterConfigFromJSONFile file = BS.readFile file >>= return . decode

instance FromJSON Person where
    parseJSON (Object v) = Person <$>
                            v .: "person_id" <*>
                            v .: "person_shortname" <*>
                            v .: "person_fullname"
    parseJSON _          = empty

importPersonsFromJSONFile :: FilePath -> IO (Maybe [Person])
importPersonsFromJSONFile file = BS.readFile file >>= return . decodePersonsFromJSON

decodePersonsFromJSON :: BS.ByteString -> Maybe [Person]
decodePersonsFromJSON = decode
