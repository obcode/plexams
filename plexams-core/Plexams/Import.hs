{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import
    ( importSemesterConfigFromYAMLFile
    , importExamsFromJSONFile
    , importExamSlotsFromYAMLFile
    , importRegistrationsFromYAMLFile
    , importOverlapsFromYAMLFile
    , parseGroup
    , importConstraintsFromYAMLFile
    , importStudentsFromYAMLFile
    , importZPAExamsFromJSONFile
    ) where

import           Control.Applicative         (empty, (<$>), (<*>))
import           Data.Aeson                  (FromJSON, Value (Object), decode,
                                              parseJSON, (.:))
import qualified Data.ByteString             as BSI
import qualified Data.ByteString.Lazy        as BS
import           Data.Char                   (digitToInt)
import           Data.List                   (elemIndex)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as S
import           Data.Time                   (Day)
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format            (defaultTimeLocale, parseTimeM)
import qualified Data.Yaml                   as Y
import           Plexams.Types

--------------------------------------------------------------------------------
-- Semesterconfig from YAML File
--------------------------------------------------------------------------------

instance Y.FromJSON SemesterConfig where
    parseJSON (Y.Object v) = makeSemesterConfig
                        <$> v Y..: "semester"
                        <*> v Y..: "firstDay"
                        <*> v Y..: "lastDay"
                        <*> v Y..: "goDay0"
                        <*> v Y..: "slotsPerDay"
                        <*> v Y..: "initialPlan"
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

makeSemesterConfig :: String -> String -> String -> String -> [String]
                   -> FilePath -> FilePath -> [AvailableRoom] -> [[Integer]]
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

--------------------------------------------------------------------------------
-- Persons from JSON File
--------------------------------------------------------------------------------

instance FromJSON Person where
    parseJSON (Object v) = Person
                        <$> v .: "person_id"
                        <*> v .: "person_shortname"
                        <*> v .: "person_fullname"
    parseJSON _          = empty

importPersonsFromJSONFile :: FilePath -> IO (Maybe Persons)
importPersonsFromJSONFile = fmap decodePersonsFromJSON . BS.readFile

decodePersonsFromJSON :: BS.ByteString -> Maybe Persons
decodePersonsFromJSON = fmap personsListToPersons . decode
  where personsListToPersons :: [Person] -> Persons
        personsListToPersons = M.fromList . map (\p -> (personID p, p))

--------------------------------------------------------------------------------
-- Exams from JSON File
--------------------------------------------------------------------------------

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
    , groupRegistrations = Nothing
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

--------------------------------------------------------------------------------
-- AddExamToSlot from YAML file
--------------------------------------------------------------------------------

listsToExamSlots :: Maybe [[Integer]] -> Maybe [AddExamToSlot]
listsToExamSlots = fmap $ map listToExamSlots

listToExamSlots :: [Integer] -> AddExamToSlot
listToExamSlots [a,d,s] = AddExamToSlot a (fromInteger d) (fromInteger s)
listToExamSlots xs      = error $ "cannot decode " ++ show xs

importExamSlotsFromYAMLFile :: FilePath -> IO (Maybe [AddExamToSlot])
importExamSlotsFromYAMLFile = fmap (listsToExamSlots . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- Registrations from YAML file
--------------------------------------------------------------------------------

data ImportRegistrations = ImportRegistrations
  { iRegGroups :: String
  , iRegs      :: [ImportRegistration]
  }

instance Y.FromJSON ImportRegistrations where
  parseJSON (Y.Object v) = ImportRegistrations
                        <$> v Y..: "group"
                        <*> v Y..: "registrations"
  parseJSON _            = empty

data ImportRegistration = ImportRegistration
  { iRegAncode :: Integer
  , iRegSum    :: Integer
  }

instance Y.FromJSON ImportRegistration where
    parseJSON (Y.Object v) = ImportRegistration
                          <$> v Y..: "ancode"
                          <*> v Y..: "sum"
    parseJSON _            = empty

listToRegistrations :: (String, [(Integer, Integer)]) -> Registrations
listToRegistrations (g, regs) = Registrations g (M.fromList regs)

iRegsToRegs :: ImportRegistrations -> Registrations
iRegsToRegs (ImportRegistrations g rs) = Registrations
  { regsGroup = g
  , regs = M.fromList $ map (\(ImportRegistration a s) -> (a, s)) rs
  }

iRegsLToRegsL = map iRegsToRegs

importRegistrationsFromYAMLFile :: FilePath -> IO (Maybe [Registrations])
importRegistrationsFromYAMLFile =
    fmap (fmap iRegsLToRegsL . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- Overlaps from YAML file
--------------------------------------------------------------------------------

data ImportOverlaps = ImportOverlaps
  { iOLGroup :: String
  , iOLList  :: [ImportOverlapsList]
  }

instance Y.FromJSON ImportOverlaps where
  parseJSON (Y.Object v) = ImportOverlaps
                        <$> v Y..: "group"
                        <*> v Y..: "overlapsList"
  parseJSON _            = empty

data ImportOverlapsList = ImportOverlapsList
  { iOLAncode :: Integer
  , iOL       :: [ImportOverlap]
  }

instance Y.FromJSON ImportOverlapsList where
  parseJSON (Y.Object v) = ImportOverlapsList
                        <$> v Y..: "ancode"
                        <*> v Y..: "overlaps"
  parseJSON _            = empty

data ImportOverlap = ImportOverlap
  { iOLOtherAncode :: Integer
  , iOLSum         :: Integer
  }

instance Y.FromJSON ImportOverlap where
    parseJSON (Y.Object v) = ImportOverlap
                          <$> v Y..: "otherExam"
                          <*> v Y..: "noOfStudents"
    parseJSON _            = empty

iOLToOL :: ImportOverlaps -> Overlaps
iOLToOL (ImportOverlaps g rs) = Overlaps
  { olGroup = read g
  , olOverlaps = M.fromList
        $ map (\(ImportOverlapsList a s) -> (a, M.fromList $ map toTupel s)) rs
  }
  where toTupel (ImportOverlap a s) = (a,s)

iOLToOLList = map iOLToOL

importOverlapsFromYAMLFile :: FilePath -> IO (Maybe [Overlaps])
importOverlapsFromYAMLFile =
    fmap (fmap iOLToOLList . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- Constraints from YAML file
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Students from YAML file
--------------------------------------------------------------------------------

data ImportStudent = ImportStudent
  { isMtkNr  :: Integer
  , isAncode :: Integer
  }

instance Y.FromJSON ImportStudent where
  parseJSON (Y.Object v) = ImportStudent
                        <$> v Y..: "mtknr"
                        <*> v Y..: "ancode"
  parseJSON _            = empty

importStudentsToStudents :: [ImportStudent] -> Students
importStudentsToStudents = foldr insertStudent M.empty
  where
    insertStudent (ImportStudent mtkNr ancode) =
      M.alter (Just . maybe (S.singleton mtkNr)
                            (S.insert mtkNr)) ancode

importStudentsFromYAMLFile :: FilePath -> IO (Maybe Students)
importStudentsFromYAMLFile =
    fmap (fmap importStudentsToStudents . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- ZPAExams from JSON file
--------------------------------------------------------------------------------

instance FromJSON ZPAExam where
    parseJSON (Object v ) = ZPAExam
                         <$> v .: "anCode"
                         <*> v .: "date"
                         <*> v .: "time"
                         <*> v .: "reserveInvigilator_id"
                         <*> v .: "rooms"
    parseJSON _          = empty

instance FromJSON ZPARoom where
    parseJSON (Object v ) = ZPARoom
                         <$> v .: "number"
                         <*> v .: "invigilator_id"
                         <*> v .: "reserveRoom"
                         <*> v .: "handicapCompensation"
                         <*> v .: "duration"
                         <*> v .: "numberStudents"
    parseJSON _          = empty

importZPAExamsFromJSONFile :: FilePath -> IO (Maybe [ZPAExam])
importZPAExamsFromJSONFile = fmap decode . BS.readFile
