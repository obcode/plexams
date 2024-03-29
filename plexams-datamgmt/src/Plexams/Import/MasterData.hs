{-# LANGUAGE OverloadedStrings #-}

module Plexams.Import.MasterData
  ( importSemesterConfigFromYAMLFile
  , importExamsFromJSONFile
  , importPersonsFromJSONFile
  , importConstraintsFromYAMLFile
  , importInvigilatorsFromJSONFile
  )
where

-- {{{ Imports
import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , empty
                                                )
import           Data.Aeson                     ( FromJSON
                                                , Value(Object)
                                                , (.:)
                                                , decode
                                                , parseJSON
                                                )
import qualified Data.ByteString               as BSI
import qualified Data.ByteString.Lazy          as BS
import           Data.List                      ( (\\) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , maybe
                                                )
import           Data.Text                      ( pack )
import qualified Data.Yaml                     as Y
import           System.Exit                    ( exitFailure )

import           Plexams.Types

-- }}}
-- {{{ Semesterconfig from YAML File
importSemesterConfigFromYAMLFile :: FilePath -> IO SemesterConfig
importSemesterConfigFromYAMLFile fp = do
  res <- Y.decodeFileWithWarnings fp
  case res of
    Left parseEx -> do
      putStrLn $ Y.prettyPrintParseException parseEx
      exitFailure
    Right (warnings, semesterconfig) -> do
      mapM_ print warnings
      return semesterconfig


-- }}}
-- {{{ Persons from JSON File
importPersonsFromJSONFile :: FilePath -> IO (Maybe Persons)
importPersonsFromJSONFile = fmap decodePersonsFromJSON . BS.readFile

decodePersonsFromJSON :: BS.ByteString -> Maybe Persons
decodePersonsFromJSON = fmap personsListToPersons . decode
 where
  personsListToPersons :: [Person] -> Persons
  personsListToPersons = M.fromList . map (\p -> (personID p, p))

-- }}}
-- {{{ Exams from JSON File
data ImportExam = ImportExam
  { ieExamType :: String
  , ieGroups :: [String]
  , ieMainExamer :: String
  , ieIsRepeaterExam :: Bool
  , ieDuration :: Integer
  , ieModule :: String
  , ieAnCode :: Integer
  , ieMainExamerId :: Integer
  }

instance FromJSON ImportExam where
  parseJSON (Object v) =
    ImportExam <$> v .: "exam_type" <*> v .: "groups" <*> v .: "main_examer" <*>
    v .: "is_repeater_exam" <*>
    v .: "duration" <*>
    v .: "module" <*>
    v .: "anCode" <*>
    v .: "main_examer_id"
  parseJSON _ = empty

importExamsFromJSONFile :: FilePath -> IO (Maybe [Exam])
importExamsFromJSONFile = fmap decodeExamsFromJSON . BS.readFile

decodeExamsFromJSON :: BS.ByteString -> Maybe [Exam]
decodeExamsFromJSON = fmap (map importExamToExam) . decode
 where
  importExamToExam :: ImportExam -> Exam
  importExamToExam ie = Exam
    { anCode                  = ieAnCode ie
    , name                    = ieModule ie
    , lecturer                = Person (ieMainExamerId ie)
                                       (pack (ieMainExamer ie))
                                       ""
                                       ""
                                       ""
                                       True
    , duration                = ieDuration ie
    , rooms                   = []
    , plannedByMe             = True
    , reExam                  = ieIsRepeaterExam ie
    , groups                  = map read $ ieGroups ie
    , examType                = ieExamType ie
    , slot                    = Nothing
    , registeredStudents      = []
    , registeredStudentsCount = 0
    , registeredGroups        = []
    , conflictingAncodes      = M.empty
    , handicapStudents        = []
    , sameRoom                = []
    , sameSlot                = []
    , shareRoom               = True
    , unregisteredStudents    = 0
    , onOtherCampus           = False
    , stakeholder             = []
    }

-- }}}
-- {{{ Constraints from YAML file
data ImportConstraints =
  ImportConstraints (Maybe [[Ancode]])
                    (Maybe [[Ancode]])
                    (Maybe [[Ancode]])
                    (Maybe [[Int]])
                    (Maybe [[Int]])
                    (Maybe [Integer])
                    (Maybe [[Int]])
                    (Maybe [[Integer]])
                    (Maybe [ImpossibleInvigilation])
                    (Maybe [RoomOnlyForSlots])
                    (Maybe [RoomOnlyForSlots])
                    (Maybe [Ancode])
                    (Maybe [[Integer]])

instance Y.FromJSON ImportConstraints where
  parseJSON (Y.Object v) =
    ImportConstraints <$> v Y..:? "notOnSameDay" <*> v Y..:? "inSameSlot" <*>
    v Y..:? "inSameRoom" <*>
    v Y..:? "onOneOfTheseDays" <*>
    v Y..:? "fixedSlot" <*>
    v Y..:? "noInvigilations" <*>
    v Y..:? "noInvigilationDays" <*>
    v Y..:? "invigilatesExam" <*>
    v Y..:? "impossibleInvigilationSlots" <*>
    v Y..:? "roomOnlyForSlots" <*>
    v Y..:? "roomNotForSlots" <*>
    v Y..:? "doNotShareRoom" <*>
    v Y..:? "unregisteredStudents"
  parseJSON _ = empty

data ImpossibleInvigilation =
  ImpossibleInvigilation Integer
                         [[Int]]

instance Y.FromJSON ImpossibleInvigilation where
  parseJSON (Y.Object v) =
    ImpossibleInvigilation <$> v Y..: "examer" <*> v Y..: "slots"
  parseJSON _ = empty

data RoomOnlyForSlots =
  RoomOnlyForSlots RoomID
                   [[Int]]

roomOnlyForSlotsToTuple :: RoomOnlyForSlots -> (RoomID, [(DayIndex, SlotIndex)])
roomOnlyForSlotsToTuple (RoomOnlyForSlots roomID' slots') =
  (roomID', map (\[dayIdx, slotIdx] -> (dayIdx, slotIdx)) slots')

roomNotForSlotsToTuple
  :: SemesterConfig -> RoomOnlyForSlots -> (RoomID, [(DayIndex, SlotIndex)])
roomNotForSlotsToTuple semesterConfig' (RoomOnlyForSlots roomID' slots') =
  ( roomID'
  , allSlots semesterConfig'
    \\ map (\[dayIdx, slotIdx] -> (dayIdx, slotIdx)) slots'
  )

instance Y.FromJSON RoomOnlyForSlots where
  parseJSON (Y.Object v) =
    RoomOnlyForSlots <$> v Y..: "roomNumber" <*> v Y..: "slots"
  parseJSON _ = empty

importConstraintsToConstraints
  :: SemesterConfig -> ImportConstraints -> Constraints
importConstraintsToConstraints semesterConfig' (ImportConstraints iCNotOnSameDay iCInSameSlot iCInSameRoom iCOnOneOfTheseDays iCFixedSlot icNoInvigilations icNoInvigilationDays iCInvigilatesExam iCImpossibleInvigilationSlots iCRoomOnlyForSlots iCRoomNotForSlots icDoNotShareRoom icUnregisteredStudents)
  = Constraints
    { overlaps = []
    , notOnSameDay = fromMaybe [] iCNotOnSameDay
    , inSameSlot = fromMaybe [] iCInSameSlot
    , inSameRoom = fromMaybe [] iCInSameRoom
    , onOneOfTheseDays = maybe []
                               (map (\(x : xs) -> (toInteger x, xs)))
                               iCOnOneOfTheseDays
    , fixedSlot = maybe []
                        (map (\[a, d, s] -> (toInteger a, (d, s))))
                        iCFixedSlot
    , noInvigilations = fromMaybe [] icNoInvigilations
    , noInvigilationDays = maybe
      []
      (concatMap (\xs -> [ (toInteger (head xs), tail xs) | not (null xs) ]))
      icNoInvigilationDays
    , invigilatesExam = maybe [] (map (\[a, p] -> (a, p))) iCInvigilatesExam
    , impossibleInvigilationSlots = maybe []
                                          (map iIToSlots)
                                          iCImpossibleInvigilationSlots
    , roomSlots = maybe M.empty
                        (M.fromList . map roomOnlyForSlotsToTuple)
                        iCRoomOnlyForSlots
      `M.union` maybe
                  M.empty
                  (M.fromList . map (roomNotForSlotsToTuple semesterConfig'))
                  iCRoomNotForSlots
    , doNotShareRoom = fromMaybe [] icDoNotShareRoom
    , examsWithUnregisteredStudents = maybe
      []
      (map (\[ac, count] -> (ac, count)))
      icUnregisteredStudents
    }

iIToSlots :: ImpossibleInvigilation -> (PersonID, [(Int, Int)])
iIToSlots (ImpossibleInvigilation p ss) = (p, map (\[d, s] -> (d, s)) ss)

importConstraintsFromYAMLFile
  :: SemesterConfig -> FilePath -> IO (Maybe Constraints)
importConstraintsFromYAMLFile sc =
  fmap (fmap (importConstraintsToConstraints sc) . Y.decodeThrow) . BSI.readFile

-- }}}
-- {{{ Invigilator from JSON File
importInvigilatorsFromJSONFile :: FilePath -> IO (Maybe [Invigilator])
importInvigilatorsFromJSONFile = fmap decode . BS.readFile
-- }}}
