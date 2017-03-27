{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import where

import           Control.Applicative  (empty, (<$>), (<*>))
import           Data.Aeson           (FromJSON, Value (Object), decode,
                                       parseJSON, (.:))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
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
makeSemesterConfig s f l = SemesterConfig s (makeDay f) (makeDay l)
    where makeDay :: String -> Day
          makeDay str = fromMaybe (error $ "cannot parse date: " ++ str)
             (parseTimeM True defaultTimeLocale "%d.%m.%Y" str)

importSemesterConfigFromJSONFile :: FilePath -> IO (Maybe SemesterConfig)
importSemesterConfigFromJSONFile file = decode <$> BS.readFile file

instance FromJSON Person where
    parseJSON (Object v) = Person <$>
                            v .: "person_id" <*>
                            v .: "person_shortname" <*>
                            v .: "person_fullname"
    parseJSON _          = empty

importPersonsFromJSONFile :: FilePath -> IO (Maybe Persons)
importPersonsFromJSONFile file =
    decodePersonsFromJSON <$> BS.readFile file

decodePersonsFromJSON :: BS.ByteString -> Maybe Persons
decodePersonsFromJSON = fmap personsListToPersons . decode
  where personsListToPersons :: [Person] -> Persons
        personsListToPersons = M.fromList . map (\p -> (personID p, p))
