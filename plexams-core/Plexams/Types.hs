module Plexams.Types
    ( Plan(..)
    , Slots
    , Slot(..)
    , Exam(..)
    , SemesterConfig(..)
    , Groups(..)
    , Person(..)
    , Persons
    , Room(..)
    ) where

import qualified Data.Map                    as M
import           Data.Time.Calendar

data SemesterConfig = SemesterConfig
    { semester    :: String   -- ^ Semester
    , firstDay    :: Day      -- ^ Erster Tag des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 10@
    , lastDay     :: Day      -- ^ Letzter Tag  des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 21@
    , examDays    :: [Day]    -- ^ vom ersten bis letzten Tag OHNE Wochenende
    , slotsPerDay :: [String] -- ^ Liste von Slots als Zeitstrings in der Form @HH:MM@. Ein Slot ist IMMER 120 Minuten lang
    }
  deriving (Eq, Show)

data Plan = Plan
    { semesterConfig   :: SemesterConfig
    , slots            :: Slots
    , unscheduledExams :: [Exam]     -- ^ Liste der Prüfungen die noch keinem Slot zugeordnet sind
    , persons          :: Persons
    }
  deriving (Show, Eq)


type Slots = M.Map (Int, Int) Slot

data Slot = Slot
    { examsInSlot        :: [Exam]
    , reserveInvigilator :: Maybe Integer  -- ^ Reserveaufsicht für die Prüfung
    }
  deriving (Show, Eq)

data Exam = Exam
    { anCode      :: Integer -- ^ Anmeldecode Prüfungsamt
    , name        :: String  -- ^ Name der Prüfung
    , lecturer    :: Person  -- ^ Prüfer
    , duration    :: Integer -- ^ Dauer der Prüfung in Minuten
    , rooms       :: [Room]  -- ^ Liste der Räume in denen die Prüfung statt findet
    , plannedByMe :: Bool    -- ^ @False@ bei Prüfungen, die zwar mit erfasst werden, aber nicht geplant werden
                             --   können
    , reExam      :: Bool    -- ^ @True@ bei einer Wiederholungsklausur
    , groups      :: Groups  -- ^ Studierendengruppen die an der Prüfung teilnehmen
    , examType    :: String  -- ^ Typ der Prüfung aus ZPA
    }
  deriving (Show, Eq)

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

data Groups = Groups
  deriving (Show, Eq)

type Persons = M.Map Integer Person

data Person = Person
    { personID        :: Integer
    , personShortName :: String
    , personFullName  :: String
    }
  deriving (Eq, Show)

