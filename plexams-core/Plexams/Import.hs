{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import
    ( importSemesterConfigFromYAMLFile
    , importPlanManipFromJSONFile
    , importExamsFromJSONFile
    , parseGroup
    ) where

import           Control.Applicative         (empty, (<$>), (<*>))
import           Data.Aeson                  (FromJSON, Value (Object), decode,
                                              parseJSON, (.:))
import qualified Data.ByteString             as BSI
import qualified Data.ByteString.Lazy        as BS
import           Data.Char                   (digitToInt)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (Day)
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format            (defaultTimeLocale, parseTimeM)
import qualified Data.Yaml                   as Y
import           Plexams.Types

instance Y.FromJSON SemesterConfig where
    parseJSON (Y.Object v) = makeSemesterConfig <$>
                            v Y..: "semester" <*>
                            v Y..: "firstDay" <*>
                            v Y..: "lastDay" <*>
                            v Y..: "slotsPerDay" <*>
                            v Y..: "initialPlan" <*>
                            v Y..: "planManip"
    parseJSON _          = empty

makeSemesterConfig :: String -> String -> String -> [String]
                    -> FilePath -> FilePath -> SemesterConfig
makeSemesterConfig s f l slots initPlan planManip =
        SemesterConfig s firstDay lastDay realExamDays slots initPlan planManip
    where makeDay :: String -> Day
          makeDay str = fromMaybe (error $ "cannot parse date: " ++ str)
             (parseTimeM True defaultTimeLocale "%d.%m.%Y" str)
          firstDay = makeDay f
          lastDay = makeDay l
          realExamDays = filter (notWeekend . toWeekDate) [firstDay..lastDay]
          notWeekend (_,_,weekday) = weekday <= 5

importSemesterConfigFromYAMLFile :: FilePath -> IO (Maybe SemesterConfig)
importSemesterConfigFromYAMLFile = fmap Y.decode . BSI.readFile

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
          , groups = map read $ ieGroups ie
          , examType = ieExamType ie
          , slot = Nothing
          }

instance Read Group where
    readsPrec _ str = [(parseGroup str, "")]

parseGroup :: String -> Group
parseGroup str = Group
    { groupDegree = str2Degree $ take 2 str
    , groupSemester = if length str > 2
                          then Just (digitToInt $ str !! 2)
                          else Nothing
    , groupSubgroup = if length str > 3
                          then Just (char2Subgroup $ str !! 3)
                          else Nothing
    }
  where
    str2Degree str = case str of
                         "IB" -> IB
                         "IC" -> IC
                         "IF" -> IF
                         "GO" -> GO
                         "IG" -> IG
                         "IN" -> IN
                         "IS" -> IS
                         _    -> error $ "unknown group: " ++ str
    char2Subgroup c = case c of
                          'A' -> A
                          'B' -> B
                          'C' -> C
                          _   -> error $ "unknown group: " ++ str

instance FromJSON PlanManip where
    parseJSON (Object v) = AddExamToSlot <$>
                            v .: "anCode" <*>
                            v .: "day" <*>
                            v .: "slot"
    parseJSON _          = empty

importPlanManipFromJSONFile :: FilePath -> IO (Maybe [PlanManip])
importPlanManipFromJSONFile = fmap decode . BS.readFile
