module Plexams.PlanManip.Exam
  ( applyAddExamToSlotListToPlan
  , addExamToSlot
  , addExamToSlot'
  ) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

import Plexams.Types

addExamToSlot ::
     Integer -- ^ Anmeldecode
  -> Int -- ^ Index des Prüfungstages, beginnend bei 0
  -> Int -- ^ Index des Prüfungsslots, beginnend bei 0
  -> Plan
  -> Plan
addExamToSlot ancode dayIdx slotIdx plan =
  let exams' = filter ((== ancode) . anCode) $ allExams plan
  in if not (null exams') && all plannedByMe exams'
       then addExamToSlot' ancode dayIdx slotIdx plan
       else plan

-- TODO: working, but has to be refactored
addExamToSlot' ::
     Integer -- ^ Anmeldecode
  -> Int -- ^ Index des Prüfungstages, beginnend bei 0
  -> Int -- ^ Index des Prüfungsslots, beginnend bei 0
  -> Plan
  -> Plan
addExamToSlot' ancode dayIdx slotIdx plan =
  let config = semesterConfig plan
      dayIdxOutOfBounds = dayIdx < 0 || dayIdx >= length (examDays config)
      slotIndexOutOfBounds =
        slotIdx < 0 || slotIdx >= length (slotsPerDay config)
        -- noch unscheduled?
      newSlots ex =
        M.update
          (\slot' ->
             Just $
             slot' {examsInSlot = M.insert (anCode ex) ex $ examsInSlot slot'})
          (dayIdx, slotIdx) $
        slots plan
        -- bereits verplant
      slotsWithExam =
        filter (\((_, _), slot') -> ancode `elem` M.keys (examsInSlot slot')) $
        M.toList $ slots plan
      slotWithExam = head slotsWithExam
      examInOtherSlot = not $ null slotsWithExam
      oldSlotWithoutExam =
        (\(k, s) -> (k, s {examsInSlot = M.delete ancode $ examsInSlot s}))
          slotWithExam
      exam = fromJust $ M.lookup ancode $ examsInSlot $ snd slotWithExam
      changedSlots =
        M.update
          (\slot' ->
             Just $
             slot'
             { examsInSlot =
                 M.insert (anCode exam) (exam {slot = Just (dayIdx, slotIdx)}) $
                 examsInSlot slot'
             })
          (dayIdx, slotIdx) $
        uncurry M.insert oldSlotWithoutExam $ slots plan
  in if dayIdxOutOfBounds || slotIndexOutOfBounds
       then plan
       else case M.lookup ancode $ unscheduledExams plan of
              Just exam' ->
                plan
                { unscheduledExams = M.delete ancode $ unscheduledExams plan
                , slots = newSlots (exam' {slot = Just (dayIdx, slotIdx)})
                }
              Nothing ->
                if examInOtherSlot
                  then plan {slots = changedSlots}
                  else plan

applyAddExamToSlotListToPlan :: Plan -> [AddExamToSlot] -> Plan
applyAddExamToSlotListToPlan plan planManips =
  foldr applyPlanManipToPlan plan $ reverse planManips
  where
    applyPlanManipToPlan (AddExamToSlot a d s) = addExamToSlot a d s
