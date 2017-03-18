module Plexams.Types
    ( makeEmptyPlan
    , Plan(..)
    , ExamDay(..)
    , isRealExamDay
    , Slot(..)
    , Exam(..)
    , SemesterConfig(..)
    ) where

import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate

data SemesterConfig = SemesterConfig
    { semester    :: String   -- ^ Semester
    , firstDay    :: Day      -- ^ Erster Tag des Prüfungszeitraumes, z.B. 'fromGregorian 2017 7 10'
    , lastDay     :: Day      -- ^ Letzter Tag  des Prüfungszeitraumes, z.B. 'fromGregorian 2017 7 21'
    , slotsPerDay :: [String] -- ^ Liste von Slots als Zeitstrings in der Form "HH:MM". Ein Slot ist IMMER 120 Minuten lang
    }

data Plan = Plan
    { semesterName :: String     -- ^ Semester, z.B. "Sommersemester 2017"
    , examDays     :: [ExamDay]  -- ^ Liste der Prüfungstage, geordnet nach Datum
    }
  deriving (Show)

data ExamDay = ExamDay
    { dateString :: String           -- ^ "DD.MM.YYYY"
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
    { timeString         :: String          -- ^ Startzeit des Slots "HH:MM"
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
    }
  where slots = map makeSlot $ slotsPerDay semesterConfig

data Exam = Exam
  deriving (Show)

