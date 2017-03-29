module Plexams.PlanManip
    ( addExamToSlot
    , makePlan
    ) where

import           Data.List     (partition)
import qualified Data.Map      as M
import           Plexams.Types

addUnscheduledExamToSlot, moveExamToSlot, addExamToSlot :: Integer -- ^ Anmeldecode
              -> Int     -- ^ Index des Prüfungstages, beginnend bei 0
              -> Int     -- ^ Index des Prüfungsslots, beginnend bei 0
              -> Plan
              -> Plan
addExamToSlot ancode dayIdx slotIdx plan =
    -- 1. Suchen ob anCode existiert und ob unscheduled oder bereits in
    -- einem Slot
    -- 2. Zu Slot hinzufügen
    -- 3. aus dem alten Slot bzw. unscheduled entfernen
    let (examL, rest) = partition ((ancode==) . anCode) $ unscheduledExams plan
        exam = head examL
        newSlots = M.update (\slot -> Just $ slot { examsInSlot = exam : examsInSlot slot })
                            (dayIdx, slotIdx) $ slots plan
    in plan { unscheduledExams = rest
            , slots = newSlots
            }

addUnscheduledExamToSlot = undefined
moveExamToSlot = undefined




{-
addExamToSlot :: Exam -> Integer -> Integer -> Plan -> Plan
addExamToSlot exam dayNo slotNo plan =
    let day = (examDays plan) !! dayNo
        slot = (slotsOfDay day) !! slotNo
        newSlot = slot { examsInSlot = exam : examsInSlot slot }
        newDay =
        examDays' = set (element dayNo)
    plan { examDays = examDays {
                               }
         }
-}

emptySlot = Slot
    { examsInSlot = []
    , reserveInvigilator = Nothing
    }

makePlan :: [Exam] -> SemesterConfig -> Maybe Persons -> Plan
makePlan exams sc = addUnscheduledExams exams . makeEmptyPlan sc

makeEmptyPlan :: SemesterConfig -> Maybe Persons -> Plan
makeEmptyPlan semesterConfig maybePers = Plan
    { semesterConfig = semesterConfig
    , slots = M.fromList
            $ zip [ (d,t) | d <- [0..length (examDays semesterConfig) - 1]
                      , t <- [0..length (slotsPerDay semesterConfig) - 1]
                  ]
                  $ repeat emptySlot
   -- examDays = map (`makeExamDay` slots)
   --                  [firstDay semesterConfig .. lastDay semesterConfig]
    , unscheduledExams = []
    , persons = maybe M.empty id maybePers
    }

addUnscheduledExams :: [Exam] -> Plan -> Plan
addUnscheduledExams exams plan = plan { unscheduledExams = exams}

