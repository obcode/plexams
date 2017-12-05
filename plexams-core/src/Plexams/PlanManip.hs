{-# LANGUAGE OverloadedStrings #-}
module Plexams.PlanManip
    ( addExamToSlot
    , applyAddExamToSlotListToPlan
    , makePlan
    , addRegistrationsListToExams
    , addStudentRegistrationsToExams
    , addStudentRegistrationsToPlan
    , applyAddRoomToExamListToPlan
    , addConstraints
    , updateExamByAncodeWith
    , setHandicapsOnScheduledExams
    , addInvigilators
    , applyAddInvigilatorsToPlan
    ) where

import           Control.Arrow      (second, (&&&))
import           Data.List          (elemIndex, nub, partition, (\\), sort)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set           as S
import           Data.Text          (Text, append, unpack)
import qualified Data.Text          as Text
import           Data.Time.Calendar
import           Data.Time.Format   (defaultTimeLocale, parseTimeM)
import           GHC.Exts           (groupWith)
import           Plexams.Query      (examDaysPerLecturer)
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
  let exams' = filter ((==ancode) . anCode) $ allExams plan
  in if not (null exams') && all plannedByMe exams'
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
          M.update (\slot' -> Just $
                              slot' { examsInSlot =
                                        M.insert (anCode ex) ex
                                        $ examsInSlot slot' })
                            (dayIdx, slotIdx) $ slots plan
        -- bereits verplant
        slotsWithExam = filter (\((_,_),slot')
                                  -> ancode `elem` M.keys (examsInSlot slot'))
                                            $ M.toList $ slots plan
        slotWithExam = head slotsWithExam
        examInOtherSlot = not $ null slotsWithExam
        oldSlotWithoutExam = (\(k,s) -> ( k
                                        , s { examsInSlot = M.delete ancode
                                                            $ examsInSlot s}))
                             slotWithExam
        exam = fromJust $ M.lookup ancode $ examsInSlot $ snd slotWithExam
        changedSlots = M.update (\slot' -> Just $
                                            slot' { examsInSlot =
                                              M.insert (anCode exam) exam
                                              $ examsInSlot slot' })
                                (dayIdx, slotIdx)
                     $ uncurry M.insert oldSlotWithoutExam $ slots plan
    in if dayIdxOutOfBounds || slotIndexOutOfBounds
       then plan
       else case M.lookup ancode $ unscheduledExams plan of
            Just exam' -> plan { unscheduledExams = M.delete ancode
                                                    $ unscheduledExams plan
                               , slots = newSlots exam'
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

-- TODO: Reserve nach oben eingeplant?
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
                , reserveRoom = not (availableRoomHandicap availableRoom) &&
                    (seatsPlanned' < registrations exam `div` 5)
                , handicapCompensation = availableRoomHandicap availableRoom
                , seatsPlanned = seatsPlanned'
                }
    -- step 3: add room to exam and put new exam into correct slot
    in plan
        { slots = M.alter
                  (\(Just slot') -> Just
                    slot'
                      { examsInSlot = M.alter (\(Just exam') -> Just
                                                exam'
                                                  { rooms = room
                                                          : rooms exam'
                                                  }
                                              )
                                              ancode
                                              $ examsInSlot slot'

                      }
                  )
                  (fromMaybe (0,0) $ slot exam)
                  $ slots plan
        }

--------------------------------------------------------------------------------
-- Add invigilators to exams and slot
--------------------------------------------------------------------------------
-- Add invigilators NOT before the room allocation is frozen

applyAddInvigilatorsToPlan :: Plan -> [AddInvigilatorToRoomOrSlot] -> Plan
applyAddInvigilatorsToPlan = foldr applyAddInvigilators
  where
    applyAddInvigilators (AddInvigilatorToRoomOrSlot personID' slot' maybeRoom)
      = addInvigilatorToExamOrSlot personID' slot' maybeRoom

addInvigilatorToExamOrSlot :: Integer -> (DayIndex, SlotIndex) -> Maybe String
                           -> Plan -> Plan
addInvigilatorToExamOrSlot personID' slotKey maybeRoom plan =
  let invigilator' = M.lookup personID' $ invigilators plan
      addInvigilator invigilator'' room exam =
        exam { rooms = map (addInvigilator' invigilator'' room )
                           $ rooms exam}
      addInvigilator' invigilator'' roomName' room' =
        if roomID room' == roomName'
        then room' { invigilator = invigilator'' }
        else room'
  in plan
      { slots = M.alter (fmap $ \slot' ->
            case maybeRoom of
              Nothing   -> slot' { reserveInvigilator = invigilator' }
              Just room -> slot'
                { examsInSlot = M.map (addInvigilator invigilator' room)
                                    $ examsInSlot slot'}
          ) slotKey $ slots plan
      }

--------------------------------------------------------------------------------
-- Make the initial plan
--------------------------------------------------------------------------------

makePlan :: [Exam] -> SemesterConfig -> Persons -> Maybe Students
         -> [Handicap] -> Plan
-- makePlan exams sc = addUnscheduledExams exams . makeEmptyPlan sc

-- makePlan :: SemesterConfig -> Maybe Persons -> Plan
makePlan exams'' semesterConfig' pers maybeStudents handicaps' =
  foldr addExamFromListToSlot
        Plan
          { semesterConfig = semesterConfig'
          , slots = slots'
          , unscheduledExams = unscheduledExams''
          , persons = pers
          , constraints = noConstraints
          , students = fromMaybe M.empty maybeStudents
          , studentsExams = mkStudentsExams maybeStudents
          , handicaps = handicaps'
          , invigilators = M.empty
          , invigilatorsPerDay = M.empty
          , initialPlan = exams'
          }
        (fk10Exams semesterConfig')
  where exams' = map (setPerson pers) exams''
        slots' = M.fromList
              $ zip [ (d,t) | d <- [0..length (examDays    semesterConfig') - 1]
                            , t <- [0..length (slotsPerDay semesterConfig') - 1]
                    ]
                    $ repeat emptySlot
        emptySlot = Slot
            { examsInSlot = M.empty
            , reserveInvigilator = Nothing
            }
        (examsPlannedNotByMe', unscheduledExams') =
                    partition ((`elem` (map head $ fk10Exams semesterConfig'))
                              . anCode) exams'
        unscheduledExams'' = M.fromList $ map (\e -> (anCode e, e))
                                              $ unscheduledExams'
                                              ++ examsPlannedNotByMe
        examsPlannedNotByMe = map (\e -> e { plannedByMe = False })
                                  examsPlannedNotByMe'
        addExamFromListToSlot [a,d,t] plan =
                addExamToSlot' a (fromInteger d) (fromInteger t) plan
        addExamFromListToSlot _       plan = plan
        mkStudentsExams Nothing = M.empty
        mkStudentsExams (Just students') =
            foldr insertAncodes M.empty $ M.toList students'
          where insertAncodes (ancode, students'') studentsExams' =
                          foldr (insertStudent ancode) studentsExams'
                                                       $ S.toList students''
                insertStudent ancode (m, n)=
                  M.alter (Just . maybe (n, S.singleton ancode)
                                        (second (S.insert ancode))) m
        setPerson :: Persons -> Exam -> Exam
        setPerson ps exam =
          let pExam = lecturer exam
              maybePerson = M.lookup (personID pExam) ps
          in case maybePerson of
            Just person | personShortName person == personShortName pExam
              -> exam { lecturer = person }
            _ -> error $ "person " ++ show (personID pExam)
                        ++ " unknown (exam "++ show (anCode exam) ++ ")"

--------------------------------------------------------------------------------
-- Add registrations
--------------------------------------------------------------------------------

addRegistrationsListToExams :: [Exam] -> [Registrations] -> [Exam]
addRegistrationsListToExams = foldr addRegistrationsToExams

addRegistrationsToExams :: Registrations -> [Exam] -> [Exam]
addRegistrationsToExams = map . addRegistrationsToExam

addRegistrationsToExam :: Registrations -> Exam -> Exam
addRegistrationsToExam registrations' exam =
  if examNotForGroup
  then exam
  else maybe exam (addRegToExam exam) regForExam
  where regForExam = M.lookup (anCode exam) (regs registrations')
        (_, otherGroups) =
          partition ((==regGroup) . groupDegree)
                  $ groups exam
        addRegToExam exam' regSum = exam'
          { groups = Group regGroup Nothing Nothing (Just regSum)
                     : otherGroups
          }
        regGroup = read $ regsGroup registrations'
        examNotForGroup = notElem (regsGroup registrations')
                        $ map (show . groupDegree)
                        $ groups exam

--------------------------------------------------------------------------------
-- Add students registrations
--------------------------------------------------------------------------------

addStudentRegistrationsToPlan :: StudentsWithRegs -> Plan -> Plan
addStudentRegistrationsToPlan studentsWithRegs plan = plan
  { slots = slots'
  , unscheduledExams = unscheduledExams'
  }
  where
    unscheduledExams' = addStudentRegistrationsToExamsMap studentsWithRegs
                        $ unscheduledExams plan
    slots' = M.map
             (\s -> s { examsInSlot = addStudentRegistrationsToExamsMap
                                      studentsWithRegs $ examsInSlot s
                      }
             ) $ slots plan

addStudentRegistrationsToExams :: StudentsWithRegs -> [Exam] -> [Exam]
addStudentRegistrationsToExams studentsWithRegs =
  M.elems
  . addStudentRegistrationsToExamsMap studentsWithRegs
  . M.fromList
  . map (\e -> (anCode e, e))

addStudentRegistrationsToExamsMap :: StudentsWithRegs
                                  -> M.Map Ancode Exam
                                  -> M.Map Ancode Exam
addStudentRegistrationsToExamsMap studentsWithRegs examsMap =
  M.map (\e -> e { conflictingAncodes =
                    filter (`elem` M.keys examsMap \\ [anCode e])
                    $ sort $ nub $conflictingAncodes e

               , registeredGroups = sumRegisteredGroups $ registeredGroups e
               })
  $ foldr insertStudentReg examsMap $ M.elems studentsWithRegs
  where
    sumRegisteredGroups regGroups =
      map (\ (RegisteredGroup gName i : gs) ->
              RegisteredGroup gName (i + sum (map registeredGroupStudents gs)))
          $ groupWith registeredGroupDegree regGroups
    insertStudentReg studentWithReg examsMap' =
      foldr
        (M.alter
          $ fmap
          $ \e -> e { registeredStudents = studentWithReg : registeredStudents e
                    , registeredGroups =
                        RegisteredGroup (studentGroup studentWithReg) 1
                        : registeredGroups e
                    , conflictingAncodes =
                        studentAncodes studentWithReg ++ conflictingAncodes e
                    })
        examsMap'
        $ studentAncodes studentWithReg

--------------------------------------------------------------------------------
-- Add constraints
--------------------------------------------------------------------------------

addConstraints :: Constraints -> Plan -> Plan
addConstraints c p = p { constraints = c }

updateExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateExamByAncodeWith plan ancode f
  | isScheduledAncode ancode plan =
                                updateScheduledExamByAncodeWith plan ancode f
  | isUnscheduledAncode ancode plan =
                                updateUnscheduledExamByAncodeWith plan ancode f
  | otherwise = plan

updateScheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateScheduledExamByAncodeWith plan ancode f =
  let updatedExamsInSlot = M.alter (fmap f) ancode
                                   $ examsInSlot oldSlotContents
          -- map (second (filter ((==ancode) . anCode) . M.elems . examsInSlot))
      (oldSlot, oldSlotContents) =
        head -- should not fail
        $ filter (elem ancode . map anCode . M.elems . examsInSlot . snd)
        $ M.toList
        $ slots plan
  in plan { slots = M.alter (fmap $ \s -> s {examsInSlot = updatedExamsInSlot})
                            oldSlot $ slots plan}

updateUnscheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateUnscheduledExamByAncodeWith plan ancode f =
  plan { unscheduledExams = M.alter (fmap f) ancode $ unscheduledExams plan}

setHandicapsOnScheduledExams :: Plan -> Plan
setHandicapsOnScheduledExams plan =
  let handicapsPerAncode =
        map (\aH -> (fst $ head aH, map snd aH))
        $ groupWith fst
        $ concatMap (\h -> map (\a -> (a,h)) $ exams h)
        $ handicaps plan
  in foldr (\(a,hs) p ->
            updateExamByAncodeWith p a (\e -> e {
              studentsWithHandicaps = hs
            })
           )
           plan
           handicapsPerAncode

addInvigilators :: [Invigilator] -> Plan -> Plan
addInvigilators invigilatorList plan =
  let makeDay :: Text -> Day
      makeDay str =
        fromMaybe (error $ unpack $ "cannot parse date: " `append` str)
                  (parseTimeM True defaultTimeLocale "%d.%m.%y" (unpack str))
      examDaysPerLecturer' = examDaysPerLecturer plan
      allDays = [0 .. maxDayIndex plan]
      examDays' invigilator' = M.findWithDefault [] (invigilatorID invigilator')
                                      examDaysPerLecturer'
      excludedDays' invigilator' = nub $
            concatMap snd (filter ((== invigilatorID invigilator') . fst)
                          (noInvigilationDays $ constraints plan))
            ++ mapMaybe (flip elemIndex (examDays $ semesterConfig plan) . makeDay)
                        (invigilatorExcludedDates invigilator')
      wantDays invigilator'= examDays' invigilator' \\ excludedDays' invigilator'
      canDays invigilator' =
        if length (wantDays invigilator') >= 3
        then []
        else (allDays \\ wantDays invigilator') \\ excludedDays' invigilator'
      addDays' invigilator' = invigilator'
        { invigilatorExamDays     = examDays' invigilator'
        , invigilatorExcludedDays = excludedDays' invigilator'
        , invigilatorWantDays     = wantDays invigilator'
        , invigilatorCanDays      = canDays invigilator'
        }
      personIsInvigilator person =
        not (personIsLBA person)
        && (personFK person == "FK07")
        && ("Prof." `Text.isPrefixOf` personFullName person)
        && (personID person `notElem` noInvigilations (constraints plan))
      addInfoOrCreate :: Person -> Maybe Invigilator -> Maybe Invigilator
      addInfoOrCreate person' Nothing = Just $ addDays' Invigilator
        { invigilatorExcludedDays         = []
        , invigilatorExamDays             = []
        , invigilatorWantDays             = []
        , invigilatorCanDays              = allDays
        , invigilatorPerson               = Just person'
        , invigilatorMinutesTodo          = 0
        , invigilatorsMinutesPlanned      = 0
        , invigilatorName                 = personShortName person'
        , invigilatorID                   = personID person'
        , invigilatorExcludedDates        = []
        , invigilatorPartTime             = 1.0
        , invigilatorFreeSemester         = 0.0
        , invigilatorOvertimeThisSemester = 0.0
        , invigilatorOvertimeLastSemester = 0.0
        , invigilatorOralExams            = 0
        , invigilatorMaster               = 0
        }
      addInfoOrCreate person' (Just invigilator') =
        Just $ invigilator' { invigilatorPerson = Just person' }
      addPersonsAndMissingInvigilators :: [Person] -> Invigilators
                                       -> Invigilators
      addPersonsAndMissingInvigilators persons' invigilators' =
        foldr (\person invigilators'' ->
            M.alter (addInfoOrCreate person) (personID person) invigilators''
          ) invigilators' $ filter personIsInvigilator persons'
  in plan
      { invigilators =
          addPersonsAndMissingInvigilators (M.elems (persons plan))
          $ M.fromList $ map (invigilatorID &&& addDays') invigilatorList
      }
