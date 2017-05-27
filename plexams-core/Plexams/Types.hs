{-# LANGUAGE DeriveGeneric #-}
module Plexams.Types
    ( Plan(..)
    , Slots
    , Slot(..)
    , DayIndex
    , maxDayIndex
    , SlotIndex
    , maxSlotIndex
    , examDateAsString
    , examSlotAsString
    , dateString
    , setSlotsOnExams
    , Exam(..)
    , registrations
    , Ancode
    , SemesterConfig(..)
    , Group(..)
    , Degree(..)
    , allDegrees
    , Subgroup(..)
    , Person(..)
    , PersonID
    , Persons
    , AvailableRoom(..)
    , Room(..)
    , RoomID
    , PlanManip(..)
    , Registrations(..)
    , Constraints(..)
    , noConstraints
    , Overlaps(..)
    , setFK10Exam
    , isScheduled
    , isUnscheduled
    , Students
    , MtkNr
    , ValidationResult(..)
    , validationResult
    , ZPAExam(..)
    , ZPARoom(..)
    ) where

import qualified Data.Map           as M
import           Data.Maybe         (isJust, mapMaybe)
import qualified Data.Set           as S
import           Data.Time.Calendar
import           GHC.Generics

data SemesterConfig = SemesterConfig
    { semester        :: String   -- ^ Semester
    , firstDay        :: Day      -- ^ Erster Tag des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 10@
    , lastDay         :: Day      -- ^ Letzter Tag  des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 21@
    , examDays        :: [Day]    -- ^ vom ersten bis letzten Tag OHNE Wochenende
    , goSlots         :: [(Int, Int)]
    , slotsPerDay     :: [String] -- ^ Liste von Slots als Zeitstrings in der Form @HH:MM@. Ein Slot ist IMMER 120 Minuten lang
    , initialPlanFile :: FilePath -- ^ Datei in der die Prüfungen für das Semester vom ZPA stehen
    , planManipFile   :: FilePath -- ^ Datei in der die Prüfungen für das Semester vom ZPA stehen
    , availableRooms  :: [AvailableRoom]
    , fk10Exams       :: [[Integer]]
    }
  deriving (Eq, Show, Generic)

data AvailableRoom = AvailableRoom
    { availableRoomName     :: String
    , availableRoomMaxSeats :: Integer
    }
  deriving (Eq, Show, Generic)

data Plan = Plan
    { semesterConfig   :: SemesterConfig
    , slots            :: Slots
    , unscheduledExams :: M.Map Ancode Exam  -- ^ Liste der Prüfungen die noch keinem Slot zugeordnet sind
                                              -- Ancode -> Exam
    , persons          :: Persons
    , constraints      :: Maybe Constraints
    , students         :: Students
    , studentsExams    :: StudentsExams
    , initialPlan      :: [Exam]
    }
  deriving (Show, Eq)

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
addSlotKeyToExam k slot =
    slot { examsInSlot = M.map (addSlotKey k) $ examsInSlot slot }
  where
    addSlotKey k exam = exam { slot = Just k }

data Slot = Slot
    { examsInSlot        :: M.Map Ancode Exam -- Ancode -> Exam
    , reserveInvigilator :: Maybe Integer  -- ^ Reserveaufsicht für die Prüfung
    }
  deriving (Show, Eq)

type Ancode = Integer
type Duration = Integer
type ExamType = String

data Exam = Exam
    { anCode      :: Ancode -- ^ Anmeldecode Prüfungsamt
    , name        :: String  -- ^ Name der Prüfung
    , lecturer    :: Person  -- ^ Prüfer
    , duration    :: Duration -- ^ Dauer der Prüfung in Minuten
    , rooms       :: [Room]  -- ^ Liste der Räume in denen die Prüfung statt findet
    , plannedByMe :: Bool    -- ^ @False@ bei Prüfungen, die zwar mit erfasst werden, aber nicht geplant werden
                             --   können
    , reExam      :: Bool    -- ^ @True@ bei einer Wiederholungsklausur
    , groups      :: [Group]  -- ^ Studierendengruppen die an der Prüfung teilnehmen
    , examType    :: ExamType  -- ^ Typ der Prüfung aus ZPA
    , slot        :: Maybe (Int, Int) -- ^ (Tag, Slot)
    }
  deriving (Eq)

isScheduled :: Exam -> Bool
isScheduled = isJust . slot

isUnscheduled :: Exam -> Bool
isUnscheduled = not . isScheduled

registrations :: Exam -> Integer
registrations = sum . mapMaybe groupRegistrations . groups

setFK10Exam :: [Ancode] -> Exam -> Exam
setFK10Exam ancodes exam =
  exam { plannedByMe = anCode exam `notElem` ancodes }

instance Show Exam where
    show exam = show (anCode exam) ++ ". "
                ++ name exam
                ++ ", " ++ personShortName (lecturer exam)
                ++ (if reExam exam then ", W " else ", E ")
                ++ show (groups exam)
                ++ (if registrations exam > 0
                    then "=" ++ show (registrations exam)
                    else "")
                ++ maybe "" ((", "++) . show) (slot exam)

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
  deriving (Show, Eq)

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

data Degree = IB | IC | IF | GO | IG | IN | IS
  deriving (Show, Eq, Ord, Read, Enum)

allDegrees :: [Degree]
allDegrees = [IB .. IS]

data Subgroup = A | B | C
  deriving (Show, Eq, Ord)

type Persons = M.Map Integer Person
type PersonID = Integer

data Person = Person
    { personID        :: PersonID
    , personShortName :: String
    , personFullName  :: String
    }
  deriving (Eq, Show, Ord)

data PlanManip =
    AddExamToSlot
      { planManipAnCode :: Integer
      , planManipDay    :: Int
      , planManipSlot   :: Int
      }
  | AddRoomToExam
      { addRoomAnCode   :: Integer
      , addRoomRoomName :: String
      }

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
type Students = M.Map Ancode (S.Set MtkNr)

type StudentsExams = M.Map MtkNr (S.Set Ancode)

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

-- ZPAExport

data ZPAExam = ZPAExam
    { zpaExamAnCode               :: Integer
    , zpaExamDate                 :: String
    , zpaExamTime                 :: String
    , zpaExamReserveInvigilatorId :: Integer
    , zpaExamRooms                :: [ZPARoom]
    }
  deriving (Generic)

data ZPARoom = ZPARoom
    { zpaRoomNumber               :: String
    , zpaRoomInvigilatorId        :: Integer
    , zpaRoomReserveRoom          :: Bool
    , zpaRoomHandicapCompensation :: Bool
    , zpaRoomDuration             :: Integer
    , zpaRoomNumberStudents       :: Integer
    }
  deriving (Generic)
