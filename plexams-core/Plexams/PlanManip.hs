module Plexams.PlanManip
    ( addExamToSlot
    , applyAddExamToSlotListToPlan
    , makePlan
    , addRegistrationsListToExams
    , applyAddRoomToExamListToPlan
    , addConstraints
    ) where

import           Data.List     (partition)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set      as S
import           Plexams.Query (allExams)
import           Plexams.Types

--------------------------------------------------------------------------------
-- Add exam to slot
--------------------------------------------------------------------------------

addExamToSlot :: Integer -- ^ Anmeldecode
              -> Int     -- ^ Index des Prüfungstages, beginnend bei 0
              -> Int     -- ^ Index des Prüfungsslots, beginnend bei 0
              -> Plan
              -> Plan
addExamToSlot ancode dayIdx slotIdx plan =
  let exams = filter ((==ancode) . anCode) $ allExams plan
  in if not (null exams) && all plannedByMe exams
     then addExamToSlot' ancode dayIdx slotIdx plan
     else plan

-- TODO: working, but has to be refactored
addExamToSlot' :: Integer -- ^ Anmeldecode
              -> Int     -- ^ Index des Prüfungstages, beginnend bei 0
              -> Int     -- ^ Index des Prüfungsslots, beginnend bei 0
              -> Plan
              -> Plan
addExamToSlot' ancode dayIdx slotIdx plan =
    let config = semesterConfig plan
        dayIdxOutOfBounds = dayIdx < 0
                          || dayIdx >= length (examDays config)
        slotIndexOutOfBounds = slotIdx < 0
                            || slotIdx >= length (slotsPerDay config)
        -- noch unscheduled?
        newSlots ex =
          M.update (\slot -> Just $
                              slot { examsInSlot =
                                        M.insert (anCode ex) ex
                                        $ examsInSlot slot })
                            (dayIdx, slotIdx) $ slots plan
        -- bereits verplant
        slotsWithExam = filter (\((_,_),slot)
                                  -> ancode `elem` M.keys (examsInSlot slot))
                                            $ M.toList $ slots plan
        slotWithExam = head slotsWithExam
        examInOtherSlot = not $ null slotsWithExam
        oldSlotWithoutExam = (\(k,s) -> ( k
                                        , s { examsInSlot = M.delete ancode
                                                            $ examsInSlot s}))
                             slotWithExam
        exam = fromJust $ M.lookup ancode $ examsInSlot $ snd slotWithExam
        changedSlots = M.update (\slot -> Just $
                                            slot { examsInSlot =
                                              M.insert (anCode exam) exam
                                              $ examsInSlot slot })
                                (dayIdx, slotIdx)
                     $ uncurry M.insert oldSlotWithoutExam $ slots plan
    in if dayIdxOutOfBounds || slotIndexOutOfBounds
       then plan
       else case M.lookup ancode $ unscheduledExams plan of
            Just exam -> plan { unscheduledExams = M.delete ancode
                                                    $ unscheduledExams plan
                              , slots = newSlots exam
                              }
            Nothing -> if examInOtherSlot
                       then plan { slots = changedSlots }
                       else plan

applyAddExamToSlotListToPlan :: Plan -> [AddExamToSlot] -> Plan
applyAddExamToSlotListToPlan plan planManips =
       foldr applyPlanManipToPlan plan $ reverse planManips
    where
        applyPlanManipToPlan (AddExamToSlot a d s) = addExamToSlot a d s

--------------------------------------------------------------------------------
-- Add room to exam
--------------------------------------------------------------------------------
-- Add room NOT before the exam schedule is frozen

applyAddRoomToExamListToPlan :: Plan -> [AddRoomToExam] -> Plan
applyAddRoomToExamListToPlan = foldr applyPlanManipToPlan
    where
        applyPlanManipToPlan (AddRoomToExam a n s dd) = addRoomToExam a n s dd


addRoomToExam :: Integer -> String -> Integer -> Maybe Integer -> Plan -> Plan
addRoomToExam ancode roomName seatsPlanned' maybeDeltaDuration plan =
  if ancode `M.member` unscheduledExams plan
  then plan -- a room cannot be added to an unscheduled exam
  else -- exam is scheduled (or unknown)
    let exam = head -- should never fail
             $ mapMaybe (M.lookup ancode . examsInSlot)
             $ M.elems
             $ slots
             $ setSlotsOnExams plan
        availableRoom = head -- should never fail
                      $ filter ((==roomName) . availableRoomName)
                      $ availableRooms $ semesterConfig plan
        room = Room
                { roomID = roomName
                , maxSeats = availableRoomMaxSeats availableRoom
                , deltaDuration = fromMaybe 0 maybeDeltaDuration
                , invigilator = Nothing
                , reserveRoom = seatsPlanned' < 3
                             && seatsPlanned' /= registrations exam
                , handicapCompensation = availableRoomHandicap availableRoom
                , seatsPlanned = seatsPlanned'
                }
    -- step 3: add room to exam and put new exam into correct slot
    in plan
        { slots = M.alter
                  (\(Just slot) -> Just
                    slot
                      { examsInSlot = M.alter (\(Just exam) -> Just
                                                exam
                                                  { rooms = room
                                                          : rooms exam
                                                  }
                                              )
                                              ancode
                                              $ examsInSlot slot

                      }
                  )
                  (fromMaybe (0,0) $ slot exam)
                  $ slots plan
        }

--------------------------------------------------------------------------------
-- Make the initial plan
--------------------------------------------------------------------------------

makePlan :: [Exam] -> SemesterConfig -> Maybe Persons -> Maybe Students -> Plan
-- makePlan exams sc = addUnscheduledExams exams . makeEmptyPlan sc

-- makePlan :: SemesterConfig -> Maybe Persons -> Plan
makePlan exams semesterConfig maybePers maybeStudents =
  foldr addExamFromListToSlot
        Plan
          { semesterConfig = semesterConfig
          , slots = slots'
          , unscheduledExams = unscheduledExams''
          , persons = fromMaybe M.empty maybePers
          , constraints = Nothing
          , students = fromMaybe M.empty maybeStudents
          , studentsExams = mkStudentsExams maybeStudents
          , initialPlan = exams
          }
        (fk10Exams semesterConfig)
  where slots' = M.fromList
              $ zip [ (d,t) | d <- [0..length (examDays    semesterConfig) - 1]
                            , t <- [0..length (slotsPerDay semesterConfig) - 1]
                    ]
                    $ repeat emptySlot
        emptySlot = Slot
            { examsInSlot = M.empty
            , reserveInvigilator = Nothing
            }
        (examsPlannedNotByMe', unscheduledExams') =
                    partition ((`elem` (map head $ fk10Exams semesterConfig))
                              . anCode) exams
        unscheduledExams'' = M.fromList $ map (\e -> (anCode e, e))
                                              $ unscheduledExams'
                                              ++ examsPlannedNotByMe
        examsPlannedNotByMe = map (\e -> e { plannedByMe = False })
                                  examsPlannedNotByMe'
        addExamFromListToSlot [a,d,t] plan =
                addExamToSlot' a (fromInteger d) (fromInteger t) plan
        addExamFromListToSlot _       plan = plan
        mkStudentsExams Nothing = M.empty
        mkStudentsExams (Just students) =
            foldr insertAncodes M.empty $ M.toList students
          where insertAncodes (ancode, students) studentsExams' =
                 foldr (insertStudent ancode) studentsExams' $ S.toList students
                insertStudent ancode =
                  M.alter (Just . maybe (S.singleton ancode)
                                        (S.insert ancode))


addUnscheduledExams :: [Exam] -> Plan -> Plan
addUnscheduledExams exams plan =
  plan { unscheduledExams = M.fromList $ map (\e -> (anCode e, e)) exams}

--------------------------------------------------------------------------------
-- Add registrations
--------------------------------------------------------------------------------

addRegistrationsListToExams :: [Exam] -> [Registrations] -> [Exam]
addRegistrationsListToExams = foldr addRegistrationsToExams

addRegistrationsToExams :: Registrations -> [Exam] -> [Exam]
addRegistrationsToExams registrations =
  map $ addRegistrationsToExam registrations

addRegistrationsToExam :: Registrations -> Exam -> Exam
addRegistrationsToExam registrations exam =
  if examNotForGroup
  then exam
  else maybe exam (addRegToExam exam) regForExam
  where regForExam = M.lookup (anCode exam) (regs registrations)
        (_, otherGroups) =
          partition ((==regGroup) . groupDegree)
                  $ groups exam
        addRegToExam exam regSum = exam
          { groups = Group regGroup Nothing Nothing (Just regSum)
                     : otherGroups
          }
        regGroup = read $ regsGroup registrations
        examNotForGroup = notElem (regsGroup registrations)
                        $ map (show . groupDegree)
                        $ groups exam

removeGroupsWithoutRegistrations :: [Exam] -> [Exam]
removeGroupsWithoutRegistrations = undefined

addConstraints :: Plan -> Constraints -> Plan
addConstraints p c = p { constraints = Just c }
