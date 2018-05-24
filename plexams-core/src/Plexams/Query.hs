module Plexams.Query
  ( queryByAnCode
  , queryByName
  , queryByLecturer
  , queryByGroup
  , queryByRegisteredGroup
  , querySlot
  , queryDay
  , examDaysPerLecturer
  , lecturerExamDays
  , examsWithSameName
  , queryStudentByName
  , queryRoomByID
  , conflictingSlotsForAncode
  ) where

import Data.List (isInfixOf, nub, sort, sortBy)
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Set (Set)
import Data.Text (unpack)
import GHC.Exts (groupWith)

import Plexams.Types

queryByAnCode :: Integer -> Plan -> [Exam]
queryByAnCode ac = filter ((== ac) . anCode) . allExams

queryByName :: String -> Plan -> [Exam]
queryByName str = filter (isInfixOf str . name) . allExams

queryByLecturer :: String -> Plan -> [Exam]
queryByLecturer str =
  filter (isInfixOf str . unpack . personShortName . lecturer) . allExams

allOrUnscheduledExams :: Bool -> Plan -> [Exam]
allOrUnscheduledExams unscheduledOnly =
  if unscheduledOnly
    then sortBy (\e1 e2 -> compare (registrations e2) (registrations e1)) .
         M.elems . unscheduledExams
    else allExams

queryByGroup :: String -> Bool -> Plan -> [Exam]
queryByGroup group unscheduledOnly =
  filter (not . null . elemOrSubGroup (parseGroup group) . groups) .
  allOrUnscheduledExams unscheduledOnly
  where
    elemOrSubGroup :: Group -> [Group] -> [Group]
    elemOrSubGroup (Group degree Nothing Nothing _) groups' =
      filter (\(Group d _ _ _) -> degree == d) groups'
    elemOrSubGroup (Group degree semester' Nothing _) groups' =
      filter (\(Group d s _ _) -> degree == d && semester' == s) groups'
    elemOrSubGroup group' groups' = filter (== group') groups'

queryByRegisteredGroup :: String -> Bool -> Plan -> [Exam]
queryByRegisteredGroup group unscheduledOnly =
  filter
    ((group `elem`) . map (unpack . registeredGroupDegree) . registeredGroups) .
  allOrUnscheduledExams unscheduledOnly

querySlot :: (Int, Int) -> Plan -> [Exam]
querySlot s = maybe [] (M.elems . examsInSlot) . M.lookup s . slots

queryDay :: Int -> Plan -> [Exam]
queryDay d plan = concatMap querySlot' slots'
  where
    slotsPerDay' = length $ slotsPerDay $ semesterConfig plan
    slots' = [(d, x) | x <- [0 .. (slotsPerDay' - 1)]]
    querySlot' s = querySlot s plan

examDaysPerLecturer :: Plan -> M.Map PersonID [DayIndex]
examDaysPerLecturer =
  M.fromList .
  map (\g -> (fst $ head g, nub $ map snd g)) .
  groupWith fst .
  concatMap
    (\((d, _), s) ->
       map (\e -> (personID $ lecturer e, d)) $ M.elems $ examsInSlot s) .
  M.toList . slots

lecturerExamDays :: Plan -> [(Person, [Int])]
lecturerExamDays =
  map (\list -> (fst (head list), map snd list)) .
  groupWith fst .
  concatMap (\((d, _), lecturers) -> map (\l -> (l, d)) lecturers) .
  M.toList . M.map (map lecturer . M.elems . examsInSlot) . slots

examsWithSameName :: Plan -> [[Exam]]
examsWithSameName = filter ((> 1) . length) . groupWith name . allExams

queryStudentByName :: String -> Plan -> [(MtkNr, (StudentName, Set Ancode))]
queryStudentByName str =
  filter (isInfixOf str . unpack . fst . snd) . M.toList . studentsExams

queryRoomByID :: String -> Plan -> PlannedRoomWithSlots
queryRoomByID roomID' plan =
  let showDay xs =
        ( examDaysAsStrings' !! fst (head xs)
        , map ((slotsAsStringsForRoom' !!) . snd) xs)
      slotsAsStringsForRoom' = slotsAsStringsForRoom $ semesterConfig plan
      examDaysAsStrings' = map (drop 5) $ examDaysAsStrings $semesterConfig plan
  in PlannedRoomWithSlots roomID' $
     map (uncurry PlannedRoomSlot . showDay) $
     groupWith fst $ queryRoomByID' roomID' plan

queryRoomByID' :: String -> Plan -> [(DayIndex, SlotIndex)]
queryRoomByID' roomID' plan =
  [ slotKey
  | (slotKey, slot') <- M.toList $ slots plan
  , roomID' `elem` concatMap (map roomID . rooms) (examsInSlot slot')
  ]

conflictingSlotsForAncode :: Ancode -> Plan -> [(Int, Int)]
conflictingSlotsForAncode ancode plan' =
  let exams ancode' = filter ((== ancode') . anCode) $ allExams plan'
      findSlotsForAncode ancode' =
        case exams ancode' of
          [] -> []
          (exam:_) ->
            maybe [] (\(d, s) -> [(d, s - 1), (d, s), (d, s + 1)]) $ slot exam
  in case exams ancode of
       [] -> []
       (exam:_) ->
         sort $
         filter ((`elem` [0 .. maxSlotIndex plan']) . snd) $
         nub $ concatMap findSlotsForAncode $ M.keys $ conflictingAncodes exam
