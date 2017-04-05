module Plexams.PlanManip
    ( addExamToSlot
    , applyPlanManipListToPlan
    , makePlan
    ) where

import           Data.List     (partition)
import qualified Data.Map      as M
import           Plexams.Types

addExamToSlot :: Integer -- ^ Anmeldecode
              -> Int     -- ^ Index des Prüfungstages, beginnend bei 0
              -> Int     -- ^ Index des Prüfungsslots, beginnend bei 0
              -> Plan
              -> Plan
addExamToSlot ancode dayIdx slotIdx plan =
    -- 1. Suchen ob anCode existiert und ob unscheduled oder bereits in
    -- einem Slot
    -- 2. Zu Slot hinzufügen
    -- 3. aus dem alten Slot bzw. unscheduled entfernen
    let -- noch unscheduled?
        (examL, unscheduledExams') = partition ((ancode==) . anCode) $ unscheduledExams plan
        newSlots ex = M.update (\slot -> Just $ slot { examsInSlot = ex : examsInSlot slot })
                            (dayIdx, slotIdx) $ slots plan
        -- bereits verplant
        slotsWithExam = filter (\((_,_),slot) -> ancode `elem` map anCode (examsInSlot slot))
                                            $ M.toList $ slots plan
        ((oldDay, oldSlot), oldSlotExams) = head slotsWithExam
        (examsFromOldSlot, restFromOldSlot) = partition ((==ancode) . anCode) $ examsInSlot oldSlotExams
        changedSlots = M.update (\slot -> Just slot { examsInSlot = restFromOldSlot})
                            (oldDay, oldSlot) $ newSlots $ head examsFromOldSlot
    in if not $ null examL
       then plan { unscheduledExams = unscheduledExams'
                 , slots = newSlots $ head examL
                 }
       else if not $ null slotsWithExam
            then plan { slots = changedSlots
                      }
            else plan

applyPlanManipListToPlan :: Plan -> [PlanManip] -> Plan
applyPlanManipListToPlan plan planManips =
       foldr applyPlanManipToPlan plan $ reverse planManips
    where
        applyPlanManipToPlan (AddExamToSlot a d s) = addExamToSlot a d s

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

