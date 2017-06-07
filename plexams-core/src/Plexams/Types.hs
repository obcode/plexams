{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Plexams.Types
    ( -- * Config
      SemesterConfig(..)
    , Constraints(..)
    , noConstraints
    , setFK10Exam

      -- * Plan
    , Plan(..)
      -- * Slots
    , Slots
    , Slot(..)
    , DayIndex
    , maxDayIndex
    , SlotIndex
    , maxSlotIndex
      -- * Exams
    , Exam(..)
    , allExams
    , scheduledExams
    , isScheduled
    , isUnscheduled
    , isUnknownExamAncode
    , withHandicaps
    , Ancode
    , examDateAsString
    , examSlotAsString
    , dateString
    , setSlotsOnExams
    , registrations
      -- * Groups
    , Group(..)
    , parseGroup
    , Degree(..)
    , allDegrees
    , Subgroup(..)
      -- * Persons (lecturer, invigilators) and students
    , Person(..)
    , PersonID
    , Persons
    , Invigilator(..)
    , Invigilators
    , addInvigilators
    , Students
    , MtkNr
    , StudentName
      -- * Rooms
    , AvailableRoom(..)
    , AvailableRooms
    , mkAvailableRooms
    , Room(..)
    , seatsMissing
    , RoomID
      -- * Plan manipulation
    , AddExamToSlot(..)
    , AddRoomToExam(..)
      -- * Registrations and overlaps
    , Overlaps(..)
    , Registrations(..)
    , Handicap(..)
    , setHandicapsOnScheduledExams
      -- * Validation
    , ValidationResult(..)
    , validationResult
      -- * ZPA
    , ZPAExam(..)
    , ZPARoom(..)
    ) where

import           Control.Applicative         (empty, (<$>), (<*>))
import           Control.Arrow               ((&&&), (***))
import           Data.Aeson                  (FromJSON, Value (Object),
                                              parseJSON, (.:))
import           Data.Char                   (digitToInt)
import           Data.List                   (elemIndex, intercalate, partition,
                                              sortBy, (\\))
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe, isJust, mapMaybe)
import           Data.Monoid                 ((<>))
import           Data.Ord                    (Down (Down), comparing)
import qualified Data.Set                    as S
import           Data.Text                   (Text, append, unpack)
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.Format            (defaultTimeLocale, parseTimeM)
import qualified Data.Yaml                   as Y
import           GHC.Exts                    (groupWith)
import           GHC.Generics
import           TextShow                    (TextShow, showb)


-- {{{ SemesterConfig
data SemesterConfig = SemesterConfig
    { semester        :: Text     -- ^ Semester
    , firstDay        :: Day      -- ^ Erster Tag des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 10@
    , lastDay         :: Day      -- ^ Letzter Tag  des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 21@
    , examDays        :: [Day]    -- ^ vom ersten bis letzten Tag OHNE Wochenende
    , goSlots         :: [(Int, Int)]
    , slotsPerDay     :: [String] -- ^ Liste von Slots als Zeitstrings in der Form @HH:MM@. Ein Slot ist IMMER 120 Minuten lang
    , initialPlanFile :: FilePath -- ^ Datei in der die Prüfungen für das Semester vom ZPA stehen
    , personsFile     :: FilePath -- ^ Datei in der die Personen für das Semester vom ZPA stehen
    , planManipFile   :: FilePath
    , availableRooms  :: [AvailableRoom]
    , fk10Exams       :: [[Integer]]
    }
  deriving (Eq, Show, Generic)

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

makeSemesterConfig :: Text -> String -> String -> String -> [String]
                   -> FilePath -> FilePath -> FilePath
                   -> [AvailableRoom] -> [[Integer]]
                   -> SemesterConfig
makeSemesterConfig s f l goDay0 =
        SemesterConfig s firstDay' lastDay' realExamDays goSlots'
    where makeDay :: String -> Day
          makeDay str = fromMaybe (error $ "cannot parse date: " ++ str)
             (parseTimeM True defaultTimeLocale "%d.%m.%Y" str)
          firstDay' = makeDay f
          lastDay' = makeDay l
          realExamDays = filter (notWeekend . toWeekDate) [firstDay'..lastDay']
          notWeekend (_,_,weekday) = weekday <= 5
          goDay0Index = fromMaybe 0 $ elemIndex (makeDay goDay0) realExamDays
          goSlots' = map (\(d,t) -> (d+goDay0Index, t)) rawGOSlots
          rawGOSlots =  [ (0,0), (0,1) -- Tag 0
                        , (1,3), (1,4), (1,5)
                        , (2,0), (2,1)
                        , (3,3), (3,4), (3,5)
                        , (4,0), (4,1)
                        , (5,3), (5,4), (5,5)
                        , (6,0), (6,1)
                        , (9,3), (9,4), (9,5)
                        ]

data AvailableRoom = AvailableRoom
    { availableRoomName     :: String
    , availableRoomMaxSeats :: Integer
    , availableRoomHandicap :: Bool
    }
  deriving (Eq, Show, Generic)

instance Y.FromJSON AvailableRoom where
    parseJSON (Y.Object v) = AvailableRoom
                       <$> v Y..: "name"
                       <*> v Y..: "seats"
                       <*> v Y..:? "handicap" Y..!= False
    parseJSON _            = empty

type AvailableRooms = M.Map (DayIndex, SlotIndex)
                            -- ( Normale Räume (absteigend sortiert nach Größe)
                            -- , Handicap Räume)
                            ([AvailableRoom], [AvailableRoom])

mkAvailableRooms :: Plan -> [AvailableRoom] -> AvailableRooms
mkAvailableRooms _ [] = M.empty
mkAvailableRooms plan rooms' =
  let slots' = M.keys $ slots plan
      (handicapCompensationRooms', normalRooms') =
                                          partition availableRoomHandicap rooms'
      normalRooms =
        sortBy (comparing (Down . availableRoomMaxSeats)) normalRooms'
      handicapCompensationRooms =
        sortBy (comparing (Down . availableRoomMaxSeats))
                handicapCompensationRooms'
      (handicapCompensationRoomOdd, handicapCompensationRoomEven) =
          (map snd *** map snd)
          $  partition fst
          $ zip (concat $ repeat [True,False]) handicapCompensationRooms
      roomSlots' = maybe M.empty roomSlots $ constraints plan
      -- normalRoomsAlways =
      --  filter (not . (`elem` M.keys roomSlots') . availableRoomName) normalRooms
      allAvailableRooms = M.fromList $ zip slots' $ concat
                        $ repeat [ (normalRooms, handicapCompensationRoomOdd)
                                 , (normalRooms, handicapCompensationRoomEven)]
      filterNotRoomID roomID' = filter ((/=roomID') . availableRoomName)
      removeRoomFromAllOtherSlots :: RoomID -> [(DayIndex, SlotIndex)] -> AvailableRooms
                      -> AvailableRooms
      removeRoomFromAllOtherSlots roomID' slots'' allRooms =
        foldr ( -- \s ->
                M.alter (maybe Nothing (\(nr, hr) -> Just
                          ( filterNotRoomID roomID' nr
                          , filterNotRoomID roomID' hr
                          )
                        ))
              ) allRooms
              $ slots' \\ slots''
   in foldr (\(r, sl) allRooms -> removeRoomFromAllOtherSlots r sl allRooms)
            allAvailableRooms $ M.toList roomSlots'
-- }}}

-- {{{ Plan
data Plan = Plan
    { semesterConfig   :: SemesterConfig
    , slots            :: Slots
    , unscheduledExams :: M.Map Ancode Exam  -- ^ Liste der Prüfungen die noch keinem Slot zugeordnet sind
                                              -- Ancode -> Exam
    , persons          :: Persons
    , constraints      :: Maybe Constraints
    , students         :: Students
    , studentsExams    :: StudentsExams
    , handicaps        :: [Handicap]
    , invigilators     :: Invigilators
    , initialPlan      :: [Exam]
    }
  deriving (Show, Eq)

scheduledExams :: Plan -> [Exam]
scheduledExams plan =
    concatMap (M.elems . examsInSlot . snd) $ M.toList $ slots plan

allExams :: Plan -> [Exam]
allExams plan = let plan' = setSlotsOnExams plan
                in M.elems (unscheduledExams plan') ++ scheduledExams plan'

isScheduledAncode :: Ancode -> Plan -> Bool
isScheduledAncode ancode =
  elem ancode
  . concatMap (M.keys . examsInSlot)
  . M.elems
  . slots

isUnscheduledAncode :: Ancode -> Plan -> Bool
isUnscheduledAncode ancode =
  elem ancode
  . M.keys
  . unscheduledExams

isUnknownExamAncode :: Ancode -> Plan -> Bool
isUnknownExamAncode ancode plan =
     not (isScheduledAncode ancode plan)
  && not (isUnscheduledAncode ancode plan)
-- }}}

-- {{{ Slots
type DayIndex = Int
type SlotIndex = Int

maxDayIndex :: Plan -> DayIndex
maxDayIndex = (\x->x-1) . length . examDays . semesterConfig

maxSlotIndex :: Plan -> SlotIndex
maxSlotIndex = (\x->x-1) . length . slotsPerDay . semesterConfig

examDateAsString :: Exam -> Plan -> String
examDateAsString exam plan =
  maybe "" (dateString . (examDays (semesterConfig plan)!!) . fst) $ slot exam

examSlotAsString :: Exam -> Plan -> String
examSlotAsString exam plan =
  maybe "" ((slotsPerDay (semesterConfig plan)!!) . snd) $ slot exam

dateString :: Day -> String
dateString day = let (y, m, d) = toGregorian day
                     showWith0 n = let s = show n
                                   in if n < 10 then "0"++s else s
                 in showWith0 d ++ "." ++ showWith0 m ++ "." ++ show y

type Slots = M.Map (DayIndex, SlotIndex) Slot

setSlotsOnExams :: Plan -> Plan
setSlotsOnExams plan = plan
    { slots = M.mapWithKey addSlotKeyToExam $ slots plan
    , unscheduledExams = M.map (\e -> e { slot = Nothing })
                             $ unscheduledExams plan
    }

addSlotKeyToExam :: (DayIndex, SlotIndex) -> Slot -> Slot
addSlotKeyToExam k slot' =
    slot' { examsInSlot = M.map (addSlotKey k) $ examsInSlot slot' }
  where
    addSlotKey k' exam = exam { slot = Just k' }

data Slot = Slot
    { examsInSlot        :: M.Map Ancode Exam -- Ancode -> Exam
    , reserveInvigilator :: Maybe Integer  -- ^ Reserveaufsicht für die Prüfung
    }
  deriving (Show, Eq)
-- }}}

-- {{{ Exam
type Ancode = Integer
type Duration = Integer
type ExamType = String

data Exam = Exam
    { anCode                :: Ancode -- ^ Anmeldecode Prüfungsamt
    , name                  :: String  -- ^ Name der Prüfung
    , lecturer              :: Person  -- ^ Prüfer
    , duration              :: Duration -- ^ Dauer der Prüfung in Minuten
    , rooms                 :: [Room]  -- ^ Liste der Räume in denen die Prüfung statt findet
    , plannedByMe           :: Bool    -- ^ @False@ bei Prüfungen, die zwar mit erfasst werden, aber nicht geplant werden
                             --   können
    , reExam                :: Bool    -- ^ @True@ bei einer Wiederholungsklausur
    , groups                :: [Group]  -- ^ Studierendengruppen die an der Prüfung teilnehmen
    , examType              :: ExamType  -- ^ Typ der Prüfung aus ZPA
    , studentsWithHandicaps :: [Handicap]
    , slot                  :: Maybe (Int, Int) -- ^ (Tag, Slot)
    }
  deriving (Eq)

isScheduled :: Exam -> Bool
isScheduled = isJust . slot

isUnscheduled :: Exam -> Bool
isUnscheduled = not . isScheduled

withHandicaps :: Exam -> Bool
withHandicaps = not . null . studentsWithHandicaps

registrations :: Exam -> Integer
registrations = sum . mapMaybe groupRegistrations . groups

setFK10Exam :: [Ancode] -> Exam -> Exam
setFK10Exam ancodes exam =
  exam { plannedByMe = anCode exam `notElem` ancodes }

instance Show Exam where
    show exam = show (anCode exam) ++ ". "
                ++ name exam
                ++ ", " ++ unpack (personShortName (lecturer exam))
                ++ (if reExam exam then ", W " else ", E ")
                ++ show (groups exam)
                ++ (if registrations exam > 0
                    then "=" ++ show (registrations exam)
                    else "")
                ++ maybe "" ((", "++) . show) (slot exam)
                ++ (if null $ studentsWithHandicaps exam then ""
                    else "\n        - "
                        ++ intercalate "\n        - "
                            (map show (studentsWithHandicaps exam))
                   )
                ++ (if null $ rooms exam then ""
                   else "\n        - "
                        ++ intercalate "\n        - " (map show (rooms exam))
                   )
-- }}}

-- type BookableRooms = M.Map String (BookableRoom, [(Integer, Integer)])

type RoomID = String

data Room = Room
    { roomID               :: RoomID        -- ^ Raum-Nr, z.B. @"R3.014"@
    , maxSeats             :: Integer       -- ^ maximale Anzahl an Prüfungsplätzen
    , deltaDuration        :: Duration      -- ^ falls der Raum für NTA genutzt wird, Anzahl der Minuten
                                            --   die die Prüfung länger
                                            --   dauert
    , invigilator          :: Maybe Integer -- ^ Aufsicht
    , reserveRoom          :: Bool          -- ^ @True@, Raum ist eingeplant, wird aber nicht im ZPA
                                            --   veröffentlicht
    , handicapCompensation :: Bool          -- ^ @True@ Raum für NTA
    , seatsPlanned         :: Integer       -- ^ Anzahl der geplanten Studierenden
    }
  deriving (Eq)

instance Show Room where
  show room = roomID room ++ ", "
           ++ show (seatsPlanned room) ++ "/" ++ show (maxSeats room)
           ++ (if reserveRoom room then ", R" else "")
           ++ (if handicapCompensation room
               then ", H (+"++ show (deltaDuration room) ++ "Min)"
               else "")

seatsMissing :: Exam -> Integer
seatsMissing exam = registrations exam
                  - sum (map seatsPlanned $ rooms exam)

data Group = Group
    { groupDegree        :: Degree
    , groupSemester      :: Maybe Int
    , groupSubgroup      :: Maybe Subgroup
    , groupRegistrations :: Maybe Integer
    }
  deriving (Eq, Ord)

instance Show Group where
    show (Group d mI mS mReg) = show d
      ++ maybe "" show mI
      ++ maybe "" show mS
      ++ maybe "" (("("++) . (++")") . show) mReg

instance TextShow Group where
  showb (Group d mI mS mReg) = showb d
    <> maybe "" showb mI
    <> maybe "" showb mS
    <> maybe "" (("("<>) . (<>")") . showb) mReg

data Degree = IB | IC | IF | GO | IG | IN | IS
  deriving (Show, Eq, Ord, Read, Enum)

instance TextShow Degree where
  showb IB = "IB"
  showb IC = "IC"
  showb IF = "IF"
  showb GO = "GO"
  showb IG = "IG"
  showb IN = "IN"
  showb IS = "IS"

allDegrees :: [Degree]
allDegrees = [IB .. IS]

data Subgroup = A | B | C
  deriving (Show, Eq, Ord)

instance TextShow Subgroup where
  showb A = "A"
  showb B = "B"
  showb C = "C"

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
    str2Degree str' = case str' of
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

type Persons = M.Map PersonID Person
type PersonID = Integer

data Person = Person
    { personID        :: PersonID
    , personShortName :: Text
    , personFullName  :: Text
    , personEmail     :: Text
    , personFK        :: Text
    , personIsLBA     :: Bool
    }
  deriving (Eq, Show, Ord)

instance TextShow Person where
  showb (Person iD shortName _ email _ _) =
    showb iD <> ". " <> showb shortName
    <> " <" <> showb email <> "> "

instance FromJSON Person where
    parseJSON (Object v) = Person
                        <$> v .: "person_id"
                        <*> v .: "person_shortname"
                        <*> v .: "person_fullname"
                        <*> v .: "email"
                        <*> v .: "fk"
                        <*> v .: "is_lba"
    parseJSON _          = empty

data AddExamToSlot =
    AddExamToSlot
      { planManipAnCode :: Integer
      , planManipDay    :: Int
      , planManipSlot   :: Int
      }

data AddRoomToExam =
    AddRoomToExam
      { addRoomAnCode        :: Integer
      , addRoomRoomName      :: String
      , addRoomSeatsPlanned  :: Integer
      , addRoomDeltaDuration :: Maybe Integer
--      , addRoomReserveRoom :: Bool -- TODO: calculate?
      }
  deriving Show

instance Y.FromJSON AddRoomToExam where
    parseJSON (Y.Object v) = AddRoomToExam
                       <$> v Y..: "ancode"
                       <*> v Y..: "room"
                       <*> v Y..: "seatsPlanned"
                       <*> v Y..:? "deltaDuration" Y..!= Nothing
    parseJSON _            = empty

data Registrations = Registrations
    { regsGroup :: String
    , regs      :: M.Map Integer Integer -- Ancode x Sum
    }
  deriving (Show)

data Constraints = Constraints
  { overlaps                    :: [Overlaps]
  , notOnSameDay                :: [[Ancode]]
  , onOneOfTheseDays            :: [(Ancode, [Int])]
  , fixedSlot                   :: [(Ancode, (Int,Int))]
  , invigilatesExam             :: [(Ancode, PersonID)]
  , impossibleInvigilationSlots :: [(PersonID, [(Int, Int)])]
  , roomSlots                   :: M.Map RoomID [(DayIndex, SlotIndex)]
  }
  deriving (Show, Eq)

noConstraints :: Constraints
noConstraints = Constraints [] [] [] [] [] [] M.empty

data Overlaps = Overlaps
  { olGroup :: Group
  , olOverlaps :: M.Map Integer    -- ancode
                        (M.Map Integer -- otherAncode
                               Integer -- noOfStudents
                         )
  }
  deriving (Show, Eq)

type MtkNr = Integer
type StudentName = Text
type Students = M.Map Ancode (S.Set (MtkNr, StudentName))

type StudentsExams = M.Map MtkNr (StudentName, S.Set Ancode)

data Handicap = Handicap
  { studentname          :: Text
  , mtknr                :: Integer
  , compensation         :: Text
  , deltaDurationPercent :: Integer
  , exams                :: [Ancode]
  , needsRoomAlone       :: Bool
  }
  deriving (Eq)

instance Y.FromJSON Handicap where
  parseJSON (Y.Object v) = Handicap
                        <$> v Y..: "studentname"
                        <*> v Y..: "mtknr"
                        <*> v Y..: "compensation"
                        <*> v Y..: "deltaDurationPercent"
                        <*> v Y..: "exams"
                        <*> v Y..:? "needsRoomAlone" Y..!= False
  parseJSON _            = empty

instance Show Handicap where
  show handicap = unpack (studentname handicap)
                  ++ " (" ++ unpack (compensation handicap) ++ ")"

updateExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateExamByAncodeWith plan ancode f
  | isScheduledAncode ancode plan =
                                updateScheduledExamByAncodeWith plan ancode f
  | isUnscheduledAncode ancode plan =
                                updateUnscheduledExamByAncodeWith plan ancode f
  | otherwise = plan

updateScheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateScheduledExamByAncodeWith plan ancode f =
  let updatedExamsInSlot = M.alter (fmap f) ancode
                                   $ examsInSlot oldSlotContents
          -- map (second (filter ((==ancode) . anCode) . M.elems . examsInSlot))
      (oldSlot, oldSlotContents) =
        head -- should not fail
        $ filter (elem ancode . map anCode . M.elems . examsInSlot . snd)
        $ M.toList
        $ slots plan
  in plan { slots = M.alter (fmap $ \s -> s {examsInSlot = updatedExamsInSlot})
                            oldSlot $ slots plan}

updateUnscheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateUnscheduledExamByAncodeWith plan ancode f =
  plan { unscheduledExams = M.alter (fmap f) ancode $ unscheduledExams plan}

setHandicapsOnScheduledExams :: Plan -> Plan
setHandicapsOnScheduledExams plan =
  let handicapsPerAncode =
        map (\aH -> (fst $ head aH, map snd aH))
        $ groupWith fst
        $ concatMap (\h -> map (\a -> (a,h)) $ exams h)
        $ handicaps plan
  in foldr (\(a,hs) p ->
            updateExamByAncodeWith p a (\e -> e {
              studentsWithHandicaps = hs
            })
           )
           plan
           handicapsPerAncode

-- {{{ Invigilator

data Invigilator = Invigilator
  { invigilatorExcludedDays         :: [Int]
  , invigilatorExamDays             :: [Int]
  , invigilatorName                 :: Text
  , invigilatorID                   :: Integer
  , invigilatorExcludedDates        :: [Text]
  , invigilatorPartTime             :: Float
  , invigilatorFreeSemester         :: Float
  , invigilatorOvertimeThisSemester :: Float
  , invigilatorOvertimeLastSemester :: Float
  , invigilatorOralExams            :: Integer
  , invigilatorMaster               :: Integer
  } deriving (Show, Eq)

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

type InvigilatorID = Integer
type Invigilators = M.Map InvigilatorID Invigilator

addInvigilators :: [Invigilator] -> Plan -> Plan
addInvigilators invigilatorList plan =
  let examDaysPerLecturer = M.fromList
        $ map (\g -> (fst $ head g, map snd g))
        $ groupWith fst
        $ concatMap (\((d,_), s) -> map (\e -> (personID $ lecturer e, d))
                                        $ M.elems
                                        $ examsInSlot s)
        $ M.toList
        $ slots plan
      makeDay :: Text -> Day
      makeDay str =
        fromMaybe (error $ unpack $ "cannot parse date: " `append` str)
                  (parseTimeM True defaultTimeLocale "%d.%m.%y" (unpack str))
      addDays' invigilator' = invigilator'
        { invigilatorExamDays = M.findWithDefault []
                                                  (invigilatorID invigilator')
                                                  examDaysPerLecturer
        , invigilatorExcludedDays =
            mapMaybe (flip elemIndex (examDays $ semesterConfig plan) . makeDay)
            $ invigilatorExcludedDates invigilator'
        }
  in plan
      { invigilators =
          M.fromList $ map (invigilatorID &&& addDays') invigilatorList
      }

-- }}}

-- {{{ Validation
data ValidationResult = EverythingOk
                      | SoftConstraintsBroken
                      | HardConstraintsBroken
  deriving (Eq, Ord)

instance Show ValidationResult where
  show EverythingOk          = "Validation ok!"
  show SoftConstraintsBroken = ">>> Soft Constraints broken <<<"
  show HardConstraintsBroken = ">>> Hard Constraints broken <<<"

validationResult :: [ValidationResult] -> ValidationResult
validationResult = maximum
-- }}}

-- {{{ ZPAExport

data ZPAExam = ZPAExam
    { zpaExamAnCode               :: Integer
    , zpaExamDate                 :: String
    , zpaExamTime                 :: String
    , zpaTotalNumber              :: Integer
    , zpaExamReserveInvigilatorId :: Integer
    , zpaExamRooms                :: [ZPARoom]
    }
  deriving (Generic)

instance FromJSON ZPAExam where
    parseJSON (Object v ) = ZPAExam
                         <$> v .: "anCode"
                         <*> v .: "date"
                         <*> v .: "time"
                         <*> v .: "total_number"
                         <*> v .: "reserveInvigilator_id"
                         <*> v .: "rooms"
    parseJSON _          = empty

data ZPARoom = ZPARoom
    { zpaRoomNumber               :: String
    , zpaRoomInvigilatorId        :: Integer
    , zpaRoomReserveRoom          :: Bool
    , zpaRoomHandicapCompensation :: Bool
    , zpaRoomDuration             :: Integer
    , zpaRoomNumberStudents       :: Integer
    }
  deriving (Generic)

instance FromJSON ZPARoom where
    parseJSON (Object v ) = ZPARoom
                         <$> v .: "number"
                         <*> v .: "invigilator_id"
                         <*> v .: "reserveRoom"
                         <*> v .: "handicapCompensation"
                         <*> v .: "duration"
                         <*> v .: "numberStudents"
    parseJSON _          = empty

-- }}}
