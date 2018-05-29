{-# LANGUAGE OverloadedStrings #-}

module Plexams.PlanManip
  ( makePlan
  , updateExamByAncodeWith
  -- , module Plexams.PlanManip.Exam
  , applyAddExamToSlotListToPlan
  , addExamToSlot
  , module Plexams.PlanManip.Invigilator
  , module Plexams.PlanManip.Room
  , module Plexams.PlanManip.StudentRegs
  ) where

import Data.List (nub, partition)
import qualified Data.Map as M

import Plexams.PlanManip.Exam
import Plexams.PlanManip.Invigilator
import Plexams.PlanManip.Room
import Plexams.PlanManip.StudentRegs
import Plexams.Types

makePlan :: [Exam] -> SemesterConfig -> Persons -> Constraints -> Plan
makePlan exams'' semesterConfig' pers constraints' =
  foldr
    addExamFromListToSlot
    Plan
    { semesterConfig = semesterConfig'
    , slots = slots'
    , unscheduledExams = unscheduledExams''
    , persons = pers
    , constraints = constraints'
    , invigilators = M.empty
    , invigilatorsPerDay = M.empty
    , initialPlan = exams'
    }
    (importedExams semesterConfig')
  where
    exams' = map (addConstraints . setPerson pers) exams''
    addConstraints exam =
      let ancode = anCode exam
          onlyOtherAncodes a as =
            filter (/= a) $ nub $ concat $ filter (a `elem`) as
          sameRoom' = onlyOtherAncodes ancode $ inSameRoom constraints'
          sameSlot' = onlyOtherAncodes ancode $ inSameSlot constraints'
      in exam {sameRoom = sameRoom', sameSlot = sameSlot'}
    slots' =
      M.fromList $
      zip
        [ (d, t)
        | d <- [0 .. length (examDays semesterConfig') - 1]
        , t <- [0 .. length (slotsPerDay semesterConfig') - 1]
        ] $
      repeat emptySlot
    emptySlot = Slot {examsInSlot = M.empty, reserveInvigilator = Nothing}
    (examsPlannedNotByMe', unscheduledExams') =
      partition
        ((`elem` (map head $ importedExams semesterConfig')) . anCode)
        exams'
    unscheduledExams'' =
      M.fromList $
      map (\e -> (anCode e, e)) $ unscheduledExams' ++ examsPlannedNotByMe
    examsPlannedNotByMe =
      map (\e -> e {plannedByMe = False}) examsPlannedNotByMe'
    addExamFromListToSlot [a, d, t] plan =
      addExamToSlot' a (fromInteger d) (fromInteger t) plan
    addExamFromListToSlot _ plan = plan
    setPerson :: Persons -> Exam -> Exam
    setPerson ps exam =
      let pExam = lecturer exam
          maybePerson = M.lookup (personID pExam) ps
      in case maybePerson of
           Just person
             | personShortName person == personShortName pExam ->
               exam {lecturer = person}
           _ ->
             error $
             "person " ++
             show (personID pExam) ++
             " unknown (exam " ++ show (anCode exam) ++ ")"

updateExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateExamByAncodeWith plan ancode f
  | isScheduledAncode ancode plan =
    updateScheduledExamByAncodeWith plan ancode f
  | isUnscheduledAncode ancode plan =
    updateUnscheduledExamByAncodeWith plan ancode f
  | otherwise = plan

updateScheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateScheduledExamByAncodeWith plan ancode f =
  let updatedExamsInSlot = M.alter (fmap f) ancode $ examsInSlot oldSlotContents
          -- map (second (filter ((==ancode) . anCode) . M.elems . examsInSlot))
      (oldSlot, oldSlotContents) =
        head -- should not fail
         $
        filter (elem ancode . map anCode . M.elems . examsInSlot . snd) $
        M.toList $ slots plan
  in plan
     { slots =
         M.alter (fmap $ \s -> s {examsInSlot = updatedExamsInSlot}) oldSlot $
         slots plan
     }

updateUnscheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateUnscheduledExamByAncodeWith plan ancode f =
  plan {unscheduledExams = M.alter (fmap f) ancode $ unscheduledExams plan}
