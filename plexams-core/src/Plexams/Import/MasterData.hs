{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import.MasterData
    ( importSemesterConfigFromYAMLFile
    , importExamsFromJSONFile
    , importPersonsFromJSONFile
    , importConstraintsFromYAMLFile
    , importInvigilatorsFromJSONFile
    ) where

-- {{{ Imports
import           Control.Applicative         (empty, (<$>), (<*>))
import           Data.Aeson                  (FromJSON, Value (Object), decode,
                                              parseJSON, (.:))
import qualified Data.ByteString             as BSI
import qualified Data.ByteString.Lazy        as BS
import           Data.List                   (elemIndex)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack)
import           Data.Time                   (Day)
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format            (defaultTimeLocale, parseTimeM)
import qualified Data.Yaml                   as Y
import           Plexams.Types
-- }}}

-- {{{ Semesterconfig from YAML File

instance Y.FromJSON SemesterConfig where
    parseJSON (Y.Object v) = makeSemesterConfig
                        <$> v Y..: "semester"
                        <*> v Y..: "firstDay"
                        <*> v Y..: "lastDay"
                        <*> v Y..: "goDay0"
                        <*> v Y..: "slotsPerDay"
                        <*> v Y..: "initialPlan"
                        <*> v Y..: "persons"
                        <*> v Y..: "planManip"
                        <*> v Y..: "rooms"
                        <*> v Y..: "notPlannedByMe"
    parseJSON _          = empty

instance Y.FromJSON AvailableRoom where
    parseJSON (Y.Object v) = AvailableRoom
                       <$> v Y..: "name"
                       <*> v Y..: "seats"
                       <*> v Y..:? "handicap" Y..!= False
    parseJSON _            = empty

makeSemesterConfig :: Text -> String -> String -> String -> [String]
                   -> FilePath -> FilePath -> FilePath
                   -> [AvailableRoom] -> [[Integer]]
                   -> SemesterConfig
makeSemesterConfig s f l goDay0 =
        SemesterConfig s firstDay lastDay realExamDays goSlots
    where makeDay :: String -> Day
          makeDay str = fromMaybe (error $ "cannot parse date: " ++ str)
             (parseTimeM True defaultTimeLocale "%d.%m.%Y" str)
          firstDay = makeDay f
          lastDay = makeDay l
          realExamDays = filter (notWeekend . toWeekDate) [firstDay..lastDay]
          notWeekend (_,_,weekday) = weekday <= 5
          goDay0Index = fromMaybe 0 $ elemIndex (makeDay goDay0) realExamDays
          goSlots = map (\(d,t) -> (d+goDay0Index, t)) rawGOSlots
          rawGOSlots =  [ (0,0), (0,1) -- Tag 0
                        , (1,3), (1,4), (1,5)
                        , (2,0), (2,1)
                        , (3,3), (3,4), (3,5)
                        , (4,0), (4,1)
                        , (5,3), (5,4), (5,5)
                        , (6,0), (6,1)
                        , (9,3), (9,4), (9,5)
                        ]

importSemesterConfigFromYAMLFile :: FilePath -> IO (Maybe SemesterConfig)
importSemesterConfigFromYAMLFile = fmap Y.decode . BSI.readFile
-- }}}

-- {{{ Persons from JSON File

instance FromJSON Person where
    parseJSON (Object v) = Person
                        <$> v .: "person_id"
                        <*> v .: "person_shortname"
                        <*> v .: "person_fullname"
                        <*> v .: "email"
                        <*> v .: "fk"
                        <*> v .: "is_lba"
    parseJSON _          = empty

importPersonsFromJSONFile :: FilePath -> IO (Maybe Persons)
importPersonsFromJSONFile = fmap decodePersonsFromJSON . BS.readFile

decodePersonsFromJSON :: BS.ByteString -> Maybe Persons
decodePersonsFromJSON = fmap personsListToPersons . decode
  where personsListToPersons :: [Person] -> Persons
        personsListToPersons = M.fromList . map (\p -> (personID p, p))
-- }}}

-- {{{ Exams from JSON File

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
    parseJSON (Object v ) = ImportExam
                         <$> v .: "exam_type"
                         <*> v .: "groups"
                         <*> v .: "main_examer"
                         <*> v .: "is_repeater_exam"
                         <*> v .: "duration"
                         <*> v .: "module"
                         <*> v .: "anCode"
                         <*> v .: "main_examer_id"
    parseJSON _          = empty

importExamsFromJSONFile :: FilePath -> IO (Maybe [Exam])
importExamsFromJSONFile = fmap decodeExamsFromJSON . BS.readFile

decodeExamsFromJSON :: BS.ByteString -> Maybe [Exam]
decodeExamsFromJSON = fmap (map importExamToExam) . decode
  where importExamToExam :: ImportExam -> Exam
        importExamToExam ie = Exam
          { anCode = ieAnCode ie
          , name = ieModule ie
          , lecturer = Person (ieMainExamerId ie) (pack (ieMainExamer ie))
                              "" "" "" True
          , duration = ieDuration ie
          , rooms = []
          , plannedByMe = True
          , reExam = ieIsRepeaterExam ie
          , groups = map read $ ieGroups ie
          , examType = ieExamType ie
          , studentsWithHandicaps = []
          , slot = Nothing
          }
-- }}}

-- {{{ Constraints from YAML file

data ImportConstraints = ImportConstraints
  { iCNotOnSameDay                :: [[Ancode]]
  , iCOnOneOfTheseDays            :: [[Int]]
  , iCFixedSlot                   :: [[Int]]
  , iCInvigilatesExam             :: [[Integer]]
  , iCImpossibleInvigilationSlots :: [ImpossibleInvigilation]
  , iCRoomOnlyForSlots            :: Maybe [RoomOnlyForSlots]
  }

instance Y.FromJSON ImportConstraints where
  parseJSON (Y.Object v) = ImportConstraints
                        <$> v Y..: "notOnSameDay"
                        <*> v Y..: "onOneOfTheseDays"
                        <*> v Y..: "fixedSlot"
                        <*> v Y..: "invigilatesExam"
                        <*> v Y..: "impossibleInvigilationSlots"
                        <*> v Y..:? "roomOnlyForSlots"
  parseJSON _            = empty

data ImpossibleInvigilation = ImpossibleInvigilation
  { iCPersonID :: Integer
  , iCSlots    :: [[Int]]
  }

instance Y.FromJSON ImpossibleInvigilation where
  parseJSON (Y.Object v) = ImpossibleInvigilation
                        <$> v Y..: "examer"
                        <*> v Y..: "slots"
  parseJSON _            = empty

data RoomOnlyForSlots = RoomOnlyForSlots
  { roomNumber    :: RoomID
  , roomOnlySlots :: [[Int]]
  }

roomOnlyForSlotsToTuple :: RoomOnlyForSlots -> (RoomID, [(DayIndex, SlotIndex)])
roomOnlyForSlotsToTuple (RoomOnlyForSlots roomID slots) =
  (roomID, map (\[dayIdx, slotIdx] -> (dayIdx, slotIdx)) slots)

instance Y.FromJSON RoomOnlyForSlots where
  parseJSON (Y.Object v) = RoomOnlyForSlots
                        <$> v Y..: "roomNumber"
                        <*> v Y..: "slots"
  parseJSON _            = empty

importConstraintsToConstraints :: ImportConstraints -> Constraints
importConstraintsToConstraints
  (ImportConstraints iCNotOnSameDay
                     iCOnOneOfTheseDays
                     iCFixedSlot
                     iCInvigilatesExam
                     iCImpossibleInvigilationSlots
                     iCRoomOnlyForSlots
  ) =
  Constraints
    { overlaps = []
    , notOnSameDay = iCNotOnSameDay
    , onOneOfTheseDays = map (\(x:xs) -> (toInteger x, xs)) iCOnOneOfTheseDays
    , fixedSlot =  map (\[a,d,s] -> (toInteger a, (d,s))) iCFixedSlot
    , invigilatesExam = map (\[a,p] -> (a,p)) iCInvigilatesExam
    , impossibleInvigilationSlots = map iIToSlots iCImpossibleInvigilationSlots
    , roomSlots = maybe M.empty (M.fromList . map roomOnlyForSlotsToTuple)
                        iCRoomOnlyForSlots
    }

iIToSlots :: ImpossibleInvigilation -> (PersonID, [(Int, Int)])
iIToSlots (ImpossibleInvigilation p ss) =
  (p, map (\[d,s] -> (d,s))  ss)

importConstraintsFromYAMLFile :: FilePath -> IO (Maybe Constraints)
importConstraintsFromYAMLFile =
    fmap (fmap importConstraintsToConstraints . Y.decode) . BSI.readFile
-- }}}

-- {{{ Invigilator from JSON File

-- {
--     "invigilator": "Braun, O.",
--     "overtime_this_semester": 0.0,
--     "free_semester": 0.0,
--     "oral_exams_contribution": 0,
--     "inviligator_id": 180,
--     "part_time": 1.0,
--     "excluded_dates": [
--         "12.07.17",
--         "14.07.17",
--         "19.07.17",
--         "21.07.17"
--     ],
--     "master_contribution": 0,
--     "overtime_last_semester": 0.0
-- }

instance FromJSON Invigilator where
    parseJSON (Object v ) = Invigilator [] []
                         <$> v .: "invigilator"
                         <*> v .: "inviligator_id"
                         <*> v .: "excluded_dates"
                         <*> v .: "part_time"
                         <*> v .: "free_semester"
                         <*> v .: "overtime_this_semester"
                         <*> v .: "overtime_last_semester"
                         <*> v .: "oral_exams_contribution"
                         <*> v .: "master_contribution"
    parseJSON _          = empty

importInvigilatorsFromJSONFile :: FilePath -> IO (Maybe [Invigilator])
importInvigilatorsFromJSONFile = fmap decode . BS.readFile
-- }}}
