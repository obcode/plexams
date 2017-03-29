module Plexams.Types
    ( makeEmptyPlan
    , addUnscheduledExams
    , Plan(..)
    , ExamDay(..)
    , realExamDays
    , isRealExamDay
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
import           Data.Time.Calendar.WeekDate

data SemesterConfig = SemesterConfig
    { semester    :: String   -- ^ Semester
    , firstDay    :: Day      -- ^ Erster Tag des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 10@
    , lastDay     :: Day      -- ^ Letzter Tag  des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 21@
    , slotsPerDay :: [String] -- ^ Liste von Slots als Zeitstrings in der Form @HH:MM@. Ein Slot ist IMMER 120 Minuten lang
    }
  deriving (Eq, Show)

data Plan = Plan
    { semesterName     :: String     -- ^ Semester, z.B. @Sommersemester 2017@
    , examDays         :: [ExamDay]  -- ^ Liste der Tage des gesamten Prüfungszeitraums, geordnet nach Datum
    , unscheduledExams :: [Exam]     -- ^ Liste der Prüfungen die noch keinem Slot zugeordnet sind
    }
  deriving (Show)

realExamDays :: Plan -> [ExamDay]
realExamDays = filter isRealExamDay . examDays

data ExamDay = ExamDay
    { dateString :: String           -- ^ @DD.MM.YYYY@
    , slotsOfDay :: [Slot]           -- ^ Liste der Slots an diesem Tag, geordnet nach Zeiten
    }
  deriving (Show)

makeExamDay :: Day -> [Slot] -> ExamDay
makeExamDay day slots = ExamDay
    { dateString = let (y, m, d) = toGregorian day
                       showWith0 n = let s = show n
                                     in if n < 10 then "0"++s else s
                   in showWith0 d ++ "." ++ showWith0 m ++ "." ++ show y
    , slotsOfDay = let (_,_,weekday) = toWeekDate day
                   in if weekday > 5 then [] else slots
    }

isRealExamDay :: ExamDay -> Bool
isRealExamDay = not . null . slotsOfDay

data Slot = Slot
    { timeString         :: String          -- ^ Startzeit des Slots @HH:MM@
    , examsInSlot        :: [Exam]
    , reserveInvigilator :: Maybe Integer  -- ^ Reserveaufsicht für die Prüfung
    }
  deriving (Show)

makeSlot time = Slot
    { timeString = time
    , examsInSlot = []
    , reserveInvigilator = Nothing
    }

makeEmptyPlan :: SemesterConfig -> Plan
makeEmptyPlan semesterConfig = Plan
    { semesterName = semester semesterConfig
    , examDays = map (`makeExamDay` slots)
                     [firstDay semesterConfig .. lastDay semesterConfig]
    , unscheduledExams = []
    }
  where slots = map makeSlot $ slotsPerDay semesterConfig

addUnscheduledExams :: Plan -> [Exam] -> Plan
addUnscheduledExams plan exams = plan { unscheduledExams = exams}

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

