{-# LANGUAGE DeriveGeneric #-}
module Plexams.Types
    ( Plan(..)
    , Slots
    , Slot(..)
    , setSlotsOnExams
    , Exam(..)
    , SemesterConfig(..)
    , Group(..)
    , Degree(..)
    , Subgroup(..)
    , Person(..)
    , Persons
    , AvailableRoom(..)
    , Room(..)
    , PlanManip(..)
    , Registrations(..)
    , Constraints(..)
    , Overlaps(..)
    , setFK10Exam
    , isScheduled
    , isUnscheduled
    ) where

import qualified Data.Map           as M
import           Data.Maybe         (isJust)
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
    , unscheduledExams :: M.Map Integer Exam  -- ^ Liste der Prüfungen die noch keinem Slot zugeordnet sind
                                              -- Ancode -> Exam
    , persons          :: Persons
    , constraints      :: Maybe Constraints
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

type Slots = M.Map (Int, Int) Slot

setSlotsOnExams :: Plan -> Plan
setSlotsOnExams plan = plan
    { slots = M.mapWithKey addSlotKeyToExam $ slots plan
    , unscheduledExams = M.map (\e -> e { slot = Nothing })
                             $ unscheduledExams plan
    }

addSlotKeyToExam :: (Int, Int) -> Slot -> Slot
addSlotKeyToExam k slot =
    slot { examsInSlot = M.map (addSlotKey k) $ examsInSlot slot }
  where
    addSlotKey k exam = exam { slot = Just k }

data Slot = Slot
    { examsInSlot        :: M.Map Integer Exam -- Ancode -> Exam
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

setFK10Exam :: [Integer] -> Exam -> Exam
setFK10Exam ancodes exam =
  exam { plannedByMe = anCode exam `notElem` ancodes }

instance Show Exam where
    show exam = show (anCode exam) ++ ". "
                ++ name exam
                ++ ", " ++ personShortName (lecturer exam)
                ++ (if reExam exam then ", W " else ", E ")
                ++ show (groups exam)
                ++ maybe "" show (slot exam)

-- type BookableRooms = M.Map String (BookableRoom, [(Integer, Integer)])

data Room = Room
    { roomID               :: String        -- ^ Raum-Nr, z.B. @"R3.014"@
    , maxSeats             :: Integer       -- ^ maximale Anzahl an Prüfungsplätzen
    , deltaDuration        :: Integer       -- ^ falls der Raum für NTA genutzt wird, Anzahl der Minuten
                                            --   die die Prüfung länger
                                            --   dauert
    , invigilator          :: Maybe Integer -- ^ Aufsicht
    , reserveRoom          :: Bool          -- ^ @True@, Raum ist eingeplant, wird aber nicht im ZPA
                                            --   veröffentlicht
    , handicapCompensation :: Bool          -- ^ @True@ Raum für NTA
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
  deriving (Show, Eq, Ord, Read)

data Subgroup = A | B | C
  deriving (Show, Eq, Ord)

type Persons = M.Map Integer Person

data Person = Person
    { personID        :: Integer
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

newtype Constraints = Constraints
  { overlaps :: [Overlaps]
  }
  deriving (Show, Eq)

data Overlaps = Overlaps
  { olGroup :: Group
  , olOverlaps :: M.Map Integer    -- ancode
                        (M.Map Integer -- otherAncode
                               Integer -- noOfStudents
                         )
  }
  deriving (Show, Eq)
