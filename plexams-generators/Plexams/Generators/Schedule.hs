module Plexams.Generators.Schedule
  ( generateSchedule
  , scheduleExamsWithSameName
  ) where

import           Control.Arrow     ((&&&))
import           Data.Maybe        (fromJust)
import           Plexams.PlanManip
import           Plexams.Query
import           Plexams.Types

generateSchedule :: Plan -> (Plan, [PlanManip])
generateSchedule plan = undefined

findSlotForUnscheduledExam :: Integer -> Plan -> PlanManip
findSlotForUnscheduledExam ancode plan = undefined

-- | Schedule exams with same name in the same slot.
--
-- This only works if one of them is already planned.
-- Ignores exams with same name that are planned in different slots.
scheduleExamsWithSameName :: Plan -> (Plan, [PlanManip])
scheduleExamsWithSameName plan =
  let exams = filter (\(s,u) -> length s == 1)
            . map (filter isScheduled &&& filter isUnscheduled)
            $ examsWithSameName
            $ setSlotsOnExams plan
      dayScheduled = fst . fromJust . slot
      slotScheduled = snd . fromJust . slot
      makePlanManips ([scheduled], unscheduled) =
        map (\exam -> AddExamToSlot (anCode exam) (dayScheduled scheduled)
                                                  (slotScheduled scheduled))
            unscheduled
      planManips = concatMap makePlanManips exams
  in --error $ show $ map (filter isScheduled &&& filter isUnscheduled)
     --              $ examsWithSameName
      --            $ setSlotsOnExams plan
     (applyPlanManipListToPlan plan planManips, planManips)
