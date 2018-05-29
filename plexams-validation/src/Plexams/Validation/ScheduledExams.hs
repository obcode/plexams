{-# LANGUAGE OverloadedStrings #-}

module Plexams.Validation.ScheduledExams
  ( validate
  ) where

import Control.Arrow ((&&&), second)
import Control.Monad.Writer
import Data.List (intersect, nub)
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)

-- import qualified Data.Set as S
import Data.Text (append)
import qualified Data.Text as Text
import GHC.Exts (groupWith)

import TextShow (showt)

import Plexams.Query
import Plexams.Types

validate :: Plan -> Writer [ValidationRecord] ValidationResult
validate plan = do
  tell [Info "## Validating Schedule"]
  constraintsOk <- validateScheduleConstraints plan
  goSlotsOk <- validateGOSlots plan
  sameNameSameSlot <- validateSameNameSameSlot plan
  conflicts <- validateConflicts plan
  -- sameSlotOverlaps <- validateOverlapsInSameSlot plan
  -- adjacentSlotOverlaps <- validateOverlapsInAdjacentSlots plan
  -- sameDayOverlaps <- validateOverlapsSameDay plan
  lecturerMaxOneExamPerSlot <- validateLecturerMaxOneExamPerSlot plan
  studentsMaxTwoExamsPerDay <- validateStudentsMaxTwoExamsPerDay plan
  return $
    validationResult
      [ constraintsOk
      , goSlotsOk
      , sameNameSameSlot
      , conflicts
            -- , sameSlotOverlaps
            -- , sameDayOverlaps
            -- , adjacentSlotOverlaps
      , lecturerMaxOneExamPerSlot
      , studentsMaxTwoExamsPerDay
      ]

validateGOSlots :: Plan -> Writer [ValidationRecord] ValidationResult
validateGOSlots plan = do
  tell [Info "### Checking GO-Slots! (hard)"]
  let allowedSlots = goSlots $ semesterConfig plan
      examsInOtherSlots =
        filter (elem "GO" . map registeredGroupDegree . registeredGroups) $
        concatMap (M.elems . examsInSlot) $
        mapMaybe (`M.lookup` slots plan) $
        filter (not . (`elem` allowedSlots)) $ M.keys $ slots plan
      goOk = null examsInOtherSlots
  unless goOk $
    forM_ examsInOtherSlots $ \exam ->
      tell
        [ HardConstraintBroken $
          "- GO exam " `append` showt (anCode exam) `append` " in wrong slot"
        ]
  return $
    if goOk
      then EverythingOk
      else HardConstraintsBroken

validateSameNameSameSlot :: Plan -> Writer [ValidationRecord] ValidationResult
validateSameNameSameSlot plan = do
  let examsGroupedByNameWithDifferentSlots =
        filter ((> 1) . length . nub . map slot) $
        groupWith name $
        filter
          (not . (`elem` concat (notOnSameDay $ constraints plan)) . anCode) $
        allExams plan
      ok = null examsGroupedByNameWithDifferentSlots
  tell [Info "### Checking exams with same name in same slot (hard)"]
  unless ok $
    mapM_
      (tell .
       (: []) .
       HardConstraintBroken .
       ("- exams with same name but different slots: " `append`) .
       showt . map (anCode &&& slot))
      examsGroupedByNameWithDifferentSlots
  return $
    if ok
      then EverythingOk
      else HardConstraintsBroken

--------------------------------------------------------------------------------
-- validate conflicts (should replace validate overlaps)
--------------------------------------------------------------------------------
validateConflicts :: Plan -> Writer [ValidationRecord] ValidationResult
validateConflicts plan = do
  tell [Info "### Checking conflicts in same slot (hard)"]
  sameSlotsOk <-
    forM
      (M.toList $ M.map (M.elems . examsInSlot) $ slots plan)
      (validateConflicts' True . (: []))
  tell [Info "### Checking conflicts in adjacent slot (hard)"]
  adjacentSlotsOk <-
    forM
      (map (map (second (M.elems . examsInSlot))) $
       adjacentSlotPairs $ slots plan) $
    validateConflicts' True
  tell [Info "### Checking conflicts on same day (soft)"]
  daySlotsOk <-
    forM (map (map (second (M.elems . examsInSlot))) $ slotsByDay $ slots plan) $
    validateConflicts' False
  return $ validationResult $ concat [sameSlotsOk, adjacentSlotsOk, daySlotsOk]

validateConflicts' ::
     Bool
  -> [((DayIndex, SlotIndex), [Exam])]
  -> Writer [ValidationRecord] ValidationResult
validateConflicts' _ [] = return EverythingOk
validateConflicts' hard [((d, s), es)] = do
  let conflicts =
        map anCode es `intersect` concatMap (M.keys . conflictingAncodes) es
  if null conflicts
    then return EverythingOk
    else do
      tell
        [ (if hard
             then HardConstraintBroken
             else SoftConstraintBroken) $
          "- Conflict in slot (" `append` showt d `append` ", " `append` showt s `append`
          "): " `append`
          Text.intercalate ", " (map showt conflicts)
        ]
      return $
        if hard
          then HardConstraintsBroken
          else SoftConstraintsBroken
validateConflicts' hard slotsAndExams = do
  let ancodes = concatMap (map anCode . snd) slotsAndExams
      conflictingancodes =
        concatMap (map (M.keys . conflictingAncodes) . snd) slotsAndExams
      conflicts = ancodes `intersect` concat conflictingancodes
      conflictingAncodesWithSlot =
        concatMap
          (\c ->
             map (second $ const c) $
             filter (elem c . map anCode . snd) slotsAndExams)
          conflicts
  if null conflicts
    then return EverythingOk
    else do
      tell [Info $ showt $ map fst slotsAndExams]
      tell
        [ (if hard
             then HardConstraintBroken
             else SoftConstraintBroken) $
          "- Conflict in slots: " `append` showt conflictingAncodesWithSlot
        ]
      return $
        if hard
          then HardConstraintsBroken
          else SoftConstraintsBroken

-- --------------------------------------------------------------------------------
-- -- validate overlaps (should be replaced by validate conflicts)
-- --------------------------------------------------------------------------------
--
-- validateOverlapsInSameSlot :: Plan -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsInSameSlot plan = do
--   tell [Info "### Checking overlaps in same slot (hard)"]
--   validateOverlaps (overlaps $ constraints plan)
--                    (map (M.elems . examsInSlot) $ M.elems $ slots plan)
--
-- validateOverlaps :: [Overlaps] -> [[Exam]]
--                  -> Writer [ValidationRecord] ValidationResult
-- validateOverlaps [] _ = do
--   tell [Info "#### no overlaps found"]
--   return EverythingOk
-- validateOverlaps overlaps' exams' =
--   validationResult <$> mapM (validateOverlapsForExams overlaps') exams'
--
-- validateOverlapsForExams :: [Overlaps] -> [Exam]
--                          -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsForExams overlaps' exams' =
--   validationResult <$> mapM (`validateOverlapsForExams'` exams') overlaps'
--
-- validateOverlapsForExams' :: Overlaps -> [Exam]
--                           -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsForExams' _ [] = return EverythingOk
-- validateOverlapsForExams' overlaps' (exam:exams') = do
--   examOk <- validateOverlapsForExam overlaps' exam exams' True
--   tailOk <- validateOverlapsForExams' overlaps' exams'
--   return $ validationResult [examOk, tailOk]
--
-- validateOverlapsInAdjacentSlots :: Plan -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsInAdjacentSlots plan = do
--   tell [Info "### Checking overlaps in adjacent slots (hard)"]
--   let maxDays  = (\x -> x-1) $ length $ examDays $ semesterConfig plan
--       maxSlots = (\x -> x-1) $ length $ slotsPerDay $ semesterConfig plan
--       daySlotPairs = [ (d,(i,j)) | d <- [0..maxDays]
--                                  , (i,j) <- zip [0..] [1..maxSlots] ]
--       examsList (d,i) =
--         M.elems $ maybe M.empty examsInSlot $ M.lookup (d,i) $ slots plan
--       exams' = map (\(d,(i,j)) ->  (examsList (d,i), examsList (d,j)))
--                   daySlotPairs
--   validateOverlapsTwoLists (overlaps $ constraints plan) exams' True
--
-- validateOverlapsSameDay :: Plan -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsSameDay plan = do
--   tell [Info "### Checking overlaps on same day (soft)"]
--   let maxDays  = (\x -> x-1) $ length $ examDays $ semesterConfig plan
--       maxSlots = (\x -> x-1) $ length $ slotsPerDay $ semesterConfig plan
--       daySlotPairs = [ (d,(i,j)) | d <- [0..maxDays]
--                                  , i <- [0..maxSlots - 2]
--                                  , j <- [i+2..maxSlots]
--                      ]
--       examsList (d,i) =
--         M.elems $ maybe M.empty examsInSlot $ M.lookup (d,i) $ slots plan
--       exams' = map (\(d,(i,j)) ->  (examsList (d,i), examsList (d,j)))
--                   daySlotPairs
--   valRes <- validateOverlapsTwoLists (overlaps $ constraints plan) exams' False
--   return $ case valRes of
--     HardConstraintsBroken -> SoftConstraintsBroken
--     _                     -> valRes
--
-- validateOverlapsTwoLists :: [Overlaps] -> [([Exam],[Exam])] -> Bool
--                  -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsTwoLists [] _ _ = do
--   tell [Info "#### no overlaps found"]
--   return EverythingOk
-- validateOverlapsTwoLists overlaps' exams' hard =
--   validationResult <$> mapM (validateOverlapsTwoListsForExams overlaps' hard) exams'
--
-- validateOverlapsTwoListsForExams :: [Overlaps] -> Bool -> ([Exam],[Exam])
--                                  -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsTwoListsForExams overlaps' hard (ex1, ex2) =
--   validationResult
--     <$> mapM (validateOverlapsTwoListsForExams' ex1 ex2 hard) overlaps'
--
-- validateOverlapsTwoListsForExams' :: [Exam] -> [Exam] -> Bool -> Overlaps
--                                   -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsTwoListsForExams' [] _ _ _ = return EverythingOk
-- validateOverlapsTwoListsForExams' (exam:exams') otherExams hard overlaps' = do
--   examOk <- validateOverlapsForExam overlaps' exam otherExams hard
--   tailOk <- validateOverlapsTwoListsForExams' exams' otherExams hard overlaps'
--   return $ validationResult [ examOk, tailOk ]
--
-- validateOverlapsForExam :: Overlaps -> Exam -> [Exam] -> Bool
--                         -> Writer [ValidationRecord] ValidationResult
-- validateOverlapsForExam _        _    []    _ = return EverythingOk
-- validateOverlapsForExam overlaps' exam exams' hard = do
--   let overlapsForExam =
--         M.findWithDefault M.empty (anCode exam) $ olOverlaps overlaps'
--       overlappingExams =
--         filter (isJust . snd)
--         $ map ((\ancode -> (ancode , M.lookup ancode overlapsForExam)) . anCode)
--               exams'
--       ok = null overlappingExams
--   forM_ overlappingExams $ \(other, Just studs) ->
--     tell [ (if hard then HardConstraintBroken else SoftConstraintBroken)
--          $ "-   " `append` showt (olGroup overlaps')
--            `append` ": Exam " `append` showt (anCode exam)
--            `append` " overlaps with exam " `append` showt other
--            `append` " for " `append` showt studs `append` " Students"]
--   return $ if ok then EverythingOk
--           else if hard then HardConstraintsBroken else SoftConstraintsBroken
--------------------------------------------------------------------------------
-- validate constraints from constraints file
--------------------------------------------------------------------------------
validateScheduleConstraints ::
     Plan -> Writer [ValidationRecord] ValidationResult
validateScheduleConstraints plan = do
  tell [Info "## Validate schedule constraints from file"]
  let constraints' = constraints plan
  notOnSameDayOk <- validateNotOnSameDay plan (notOnSameDay constraints')
  inSameSlotOk <- validateInSameSlot plan (inSameSlot constraints')
  onOneOfTheseDaysOk <-
    validateOneOfTheseDays plan (onOneOfTheseDays constraints')
  _ <- validateFixSlot plan (fixedSlot constraints')
  -- tell [HardConstraintBroken "TODO: invigilatesExam"]
  -- tell [HardConstraintBroken "TODO: impossibleInvigilationSlots"]
  return $ validationResult [notOnSameDayOk, inSameSlotOk, onOneOfTheseDaysOk]

------------------
-- not on same day
------------------
validateNotOnSameDay ::
     Plan -> [[Ancode]] -> Writer [ValidationRecord] ValidationResult
validateNotOnSameDay _ [] = return EverythingOk
validateNotOnSameDay plan listOfAncodes = do
  tell [Info "### Validate exams not on same day"]
  validationResult <$> mapM (validateNotOnSameDay' plan) listOfAncodes

validateNotOnSameDay' ::
     Plan -> [Ancode] -> Writer [ValidationRecord] ValidationResult
validateNotOnSameDay' plan ancodes = do
  let ancodesAndDays =
        filter (isJust . snd) $
        map
          (\ancode ->
             ( ancode
             , let exams' = queryByAnCode ancode plan
                   maybeDay = fst <$> slot (head exams')
               in if null exams'
                    then Nothing
                    else maybeDay))
          ancodes
      ancodesAndDaysOk =
        (== length ancodesAndDays) $ length $ nub $ map snd ancodesAndDays
  unless ancodesAndDaysOk $
    tell
      [SoftConstraintBroken $ "- not okay for " `append` showt ancodesAndDays]
  return $
    if ancodesAndDaysOk
      then EverythingOk
      else SoftConstraintsBroken

--------------------
-- one of these days
--------------------
validateOneOfTheseDays ::
     Plan -> [(Ancode, [Int])] -> Writer [ValidationRecord] ValidationResult
validateOneOfTheseDays _ [] = return EverythingOk
validateOneOfTheseDays plan ancodesAndDays = do
  tell [Info "### Validate exams on one of these days"]
  validationResult <$> mapM (validateOneOfTheseDays' plan) ancodesAndDays

validateOneOfTheseDays' ::
     Plan -> (Ancode, [Int]) -> Writer [ValidationRecord] ValidationResult
validateOneOfTheseDays' plan (ancode, days) = do
  let exams' = queryByAnCode ancode plan
      oneOfTheseDaysOk =
        case slot $ head exams' of
          Nothing -> True
          Just (d, _) -> d `elem` days
  if null exams'
    then tell [Info $ "- exam " `append` showt ancode `append` " not found"] >>
         return EverythingOk
    else do
      unless oneOfTheseDaysOk $
        tell
          [ HardConstraintBroken $
            "- exam " `append` showt ancode `append` " not on one of " `append`
            showt days
          ]
      return $
        if oneOfTheseDaysOk
          then EverythingOk
          else HardConstraintsBroken

-------------
-- fixed slot
-------------
validateFixSlot ::
     Plan
  -> [(Ancode, (Int, Int))]
  -> Writer [ValidationRecord] ValidationResult
validateFixSlot _ [] = return EverythingOk
validateFixSlot plan ancodesAndSlots = do
  tell [Info "### Validate exams in fixed slot"]
  validationResult <$> mapM (validateFixSlot' plan) ancodesAndSlots

validateFixSlot' ::
     Plan -> (Ancode, (Int, Int)) -> Writer [ValidationRecord] ValidationResult
validateFixSlot' plan (ancode, (d, s)) = do
  let exams' = queryByAnCode ancode plan
      fixedSlotOk =
        case slot $ head exams' of
          Nothing -> True
          Just (d', s') -> d' == d && s' == s
  if null exams'
    then tell [Info $ "- exam " `append` showt ancode `append` " not found"] >>
         return EverythingOk
    else do
      unless fixedSlotOk $
        tell
          [ HardConstraintBroken $
            "- exam " `append` showt ancode `append` " not in slot " `append`
            showt (d, s)
          ]
      return $
        if fixedSlotOk
          then EverythingOk
          else HardConstraintsBroken

-------------
-- in sams slot
-------------
validateInSameSlot ::
     Plan -> [[Ancode]] -> Writer [ValidationRecord] ValidationResult
validateInSameSlot _ [] = return EverythingOk
validateInSameSlot plan ancodes = do
  tell [Info "### Validate exams that should be in same slot"]
  validationResult <$> mapM (validateInSameSlot' plan) ancodes

validateInSameSlot' ::
     Plan -> [Ancode] -> Writer [ValidationRecord] ValidationResult
validateInSameSlot' plan ancodes = do
  let exams' = concatMap (`queryByAnCode` plan) ancodes
      allInSameSlot = (== 1) $ length $ nub $ map slot exams'
  if allInSameSlot
    then return EverythingOk
    else do
      tell
        [ HardConstraintBroken $
          "- exams " `append` showt ancodes `append` " not in same slot "
        ]
      return HardConstraintsBroken

-------------------------------------------------------
-- each lecturer should have zero or one exams per slot
-------------------------------------------------------
validateLecturerMaxOneExamPerSlot ::
     Plan -> Writer [ValidationRecord] ValidationResult
validateLecturerMaxOneExamPerSlot plan = do
  tell [Info "### Validate numbers of exams per lecturer per slot (soft)"]
  let listsOfLecturers =
        map
          (\(s, es) ->
             ( s
             , map lecturer $
               filter
                 ((`notElem` (concat $ inSameSlot $ constraints plan)) . anCode) $
               M.elems $ examsInSlot es)) $
        M.toList $ slots plan
      slotsWithDuplicates =
        filter (((/=) <$> nub <*> id) . snd) listsOfLecturers
  forM_ slotsWithDuplicates $ \(_, lecturers) ->
    tell
      [ HardConstraintBroken $
        "-   lecturer has more then one exam in slot: " `append` showt lecturers
      ]
  return $
    if null slotsWithDuplicates
      then EverythingOk
      else HardConstraintsBroken

-------------------------------------------------
-- each student should have two exams max per day
-------------------------------------------------
-- TODO: Unterscheidung ob zwei Erstprüfungen an einem Tag oder Wiederholungs-
-- prüfungen
validateStudentsMaxTwoExamsPerDay ::
     Plan -> Writer [ValidationRecord] ValidationResult
validateStudentsMaxTwoExamsPerDay _ = do
  tell [SoftConstraintBroken "not testing exams for students atm, TODO"]
  return SoftConstraintsBroken
-- plan
--   -- TODO: hard/soft okay?
--  = do
--   tell
--     [ Info $
--       "### Validate numbers of exams per student per day" `append`
--       " (2 are soft, 3 are hard, but not if noOfExams > 13)"
--     ]
--   let examsPerDays :: [[Ancode]]
--       examsPerDays =
--         map
--           (\d ->
--              concatMap (M.keys . examsInSlot) $
--              mapMaybe
--                (\s -> M.lookup (d, s) (slots plan))
--                [0 .. maxSlotIndex plan])
--           [0 .. maxDayIndex plan]
--       studentsWithMoreThanOneExamPerDay =
--         map mkTuples $ groupWith head $ concatMap checkStudents examsPerDays
--       checkStudents :: [Ancode] -> [[MtkNr]]
--       checkStudents =
--         filter ((> 1) . length) .
--         groupWith id .
--         concatMap (map fst . S.toList) .
--         mapMaybe (\a -> M.lookup a (students plan))
--       mkTuples :: [[MtkNr]] -> (MtkNr, [Int])
--       mkTuples mtknr' = (head $ head mtknr', map length mtknr')
--       -- oneHasThree = filter (\(_, examsPerDay) -> any (>2) examsPerDay)
--       --                       studentsWithMoreThanOneExamPerDay
--       allAncodes = map anCode $ allExams plan
--   forM_ studentsWithMoreThanOneExamPerDay $ \(mtknr', noOfExams) -> do
--     let exams' =
--           maybe [] (filter (`elem` allAncodes) . S.toList . snd) $
--           M.lookup mtknr' $ studentsExams plan
--     let three = any (> 2) noOfExams
--     tell
--       [ SoftConstraintBroken $
--         (if three
--            then "-   Student has three or more exams a day:"
--            else "-   Student has two exams per day: ") `append`
--         showt noOfExams `append`
--         " of " `append`
--         showt (length exams') `append`
--         ", MtkNr " `append`
--         showt mtknr' `append`
--         "  \n" `append`
--         showt exams'
--       ]
--   return $
--     if null studentsWithMoreThanOneExamPerDay
--       then EverythingOk
--       else SoftConstraintsBroken {- if null oneHasThree
--                 then SoftConstraintsBroken
--                 else let nosOfExams = map length
--                            $ mapMaybe ((`M.lookup` studentsExams plan) . fst)
--                                       oneHasThree
--                      in if all (>13) nosOfExams
--                         then SoftConstraintsBroken
--                         else HardConstraintsBroken -}
