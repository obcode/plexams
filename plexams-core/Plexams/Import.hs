{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import where

import           Control.Applicative         (empty, (<$>), (<*>))
import           Data.Aeson                  (FromJSON, Value (Object), decode,
                                              parseJSON, (.:))
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (Day)
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format            (defaultTimeLocale, parseTimeM)
import           Plexams.Types

instance FromJSON SemesterConfig where
    parseJSON (Object v) = makeSemesterConfig <$>
                            v .: "semester" <*>
                            v .: "firstDay" <*>
                            v .: "lastDay" <*>
                            v .: "slotsPerDay"
    parseJSON _          = empty

makeSemesterConfig :: String -> String -> String -> [String] -> SemesterConfig
makeSemesterConfig s f l = SemesterConfig s firstDay lastDay realExamDays
    where makeDay :: String -> Day
          makeDay str = fromMaybe (error $ "cannot parse date: " ++ str)
             (parseTimeM True defaultTimeLocale "%d.%m.%Y" str)
          firstDay = makeDay f
          lastDay = makeDay l
          realExamDays = filter (notWeekend . toWeekDate) [firstDay..lastDay]
          notWeekend (_,_,weekday) = weekday <= 5

importSemesterConfigFromJSONFile :: FilePath -> IO (Maybe SemesterConfig)
importSemesterConfigFromJSONFile = fmap decode . BS.readFile

instance FromJSON Person where
    parseJSON (Object v) = Person <$>
                            v .: "person_id" <*>
                            v .: "person_shortname" <*>
                            v .: "person_fullname"
    parseJSON _          = empty

importPersonsFromJSONFile :: FilePath -> IO (Maybe Persons)
importPersonsFromJSONFile = fmap decodePersonsFromJSON . BS.readFile

decodePersonsFromJSON :: BS.ByteString -> Maybe Persons
decodePersonsFromJSON = fmap personsListToPersons . decode
  where personsListToPersons :: [Person] -> Persons
        personsListToPersons = M.fromList . map (\p -> (personID p, p))

data ImportExam = ImportExam
    { ieExamType       :: String
    , ieGroups         :: [String]
    , ieMainExamer     :: String
    , ieIsRepeaterExam :: Bool
    , ieDuration       :: Integer
    , ieModule         :: String
    , ieAnCode         :: Integer
    , ieMainExamerId   :: Integer
    }

instance FromJSON ImportExam where
    parseJSON (Object v ) = ImportExam <$>
                             v .: "exam_type" <*>
                             v .: "groups" <*>
                             v .: "main_examer" <*>
                             v .: "is_repeater_exam" <*>
                             v .: "duration" <*>
                             v .: "module" <*>
                             v .: "anCode" <*>
                             v .: "main_examer_id"
    parseJSON _          = empty

importExamsFromJSONFile :: FilePath -> IO (Maybe [Exam])
importExamsFromJSONFile = fmap decodeExamsFromJSON . BS.readFile

decodeExamsFromJSON :: BS.ByteString -> Maybe [Exam]
decodeExamsFromJSON = fmap (map importExamToExam) . decode
  where importExamToExam :: ImportExam -> Exam
        importExamToExam ie = Exam
          { anCode = ieAnCode ie
          , name = ieModule ie
          , lecturer = Person (ieMainExamerId ie) (ieMainExamer ie) ""
          , duration = ieDuration ie
          , rooms = []
          , plannedByMe = True
          , reExam = ieIsRepeaterExam ie
          , groups = Groups -- TODO
          , examType = ieExamType ie
          }
