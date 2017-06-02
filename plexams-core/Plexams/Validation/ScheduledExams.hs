module Plexams.Validation.ScheduledExams
  ( validate
  ) where

import           Control.Arrow        ((&&&))
import           Control.Monad.Writer
import           Data.List            (nub)
import qualified Data.Map             as M
import           Data.Maybe           (isJust, mapMaybe)
import qualified Data.Set             as S
import           GHC.Exts             (groupWith)
import           Plexams.Query
import           Plexams.Types

validate :: Plan -> Writer [String] ValidationResult
validate plan = do
  constraintsOk <- validateScheduleConstraints plan
  tell ["## Validating Schedule"]
  goSlotsOk <- validateGOSlots plan
  sameNameSameSlot <- validateSameNameSameSlot plan
  sameSlotOverlaps <- validateOverlapsInSameSlot plan
  adjacentSlotOverlaps <- validateOverlapsInAdjacentSlots plan
  sameDayOverlaps <- validateOverlapsSameDay plan
  lecturerMaxOneExamPerSlot <- validateLecturerMaxOneExamPerSlot plan
  studentsMaxTwoExamsPerDay <- validateStudentsMaxTwoExamsPerDay plan
  return $ validationResult
            [ goSlotsOk
            , sameNameSameSlot
            , sameSlotOverlaps
            , adjacentSlotOverlaps
            , lecturerMaxOneExamPerSlot
            , studentsMaxTwoExamsPerDay
            ]

validateGOSlots :: Plan -> Writer [String] ValidationResult
validateGOSlots plan = do
  tell ["### Checking GO-Slots! (hard)"]
  let allowedSlots = goSlots $ semesterConfig plan
      examsInOtherSlots = filter (elem GO . map groupDegree . groups)
                        $ concatMap (M.elems . examsInSlot)
                        $ mapMaybe (`M.lookup` slots plan)
                        $ filter (not . (`elem` allowedSlots))
                        $ M.keys $ slots plan
      goOk = null examsInOtherSlots
  unless goOk $
    forM_ examsInOtherSlots $ \exam ->
      tell [ "- GO exam " ++ show (anCode exam) ++ " in wrong slot" ]
  return $ if goOk then EverythingOk else HardConstraintsBroken

validateSameNameSameSlot :: Plan -> Writer [String] ValidationResult
validateSameNameSameSlot plan = do
  let examsGroupedByNameWithDifferentSlots =
            filter ((>1) . length . nub . map slot)
            $ groupWith name
            $ filter (not
                     . (`elem` (maybe [] (concat . notOnSameDay)
                                      $ constraints plan))
                     . anCode
                     )
            $ allExams plan
      ok = null examsGroupedByNameWithDifferentSlots
  tell ["### Checking exams with same name in same slot (soft)"]
  unless ok $
    mapM_ ( tell
            . (:[])
            . ("- exams with same name but different slots: "++)
            . show
            . map (anCode &&& slot)
          )
          examsGroupedByNameWithDifferentSlots
  return $ if ok then EverythingOk else SoftConstraintsBroken

validateOverlapsInSameSlot :: Plan -> Writer [String] ValidationResult
validateOverlapsInSameSlot plan = do
  tell ["### Checking overlaps in same slot (hard)"]
  validateOverlaps (overlaps <$> constraints plan)
                   (map (M.elems . examsInSlot) $ M.elems $ slots plan)

validateOverlaps :: Maybe [Overlaps] -> [[Exam]]
                 -> Writer [String] ValidationResult
validateOverlaps Nothing _ = do
  tell ["#### no overlaps found"]
  return EverythingOk
validateOverlaps (Just overlaps) exams =
  validationResult <$> mapM (validateOverlapsForExams overlaps) exams

validateOverlapsForExams :: [Overlaps] -> [Exam]
                         -> Writer [String] ValidationResult
validateOverlapsForExams overlaps exams =
  validationResult <$> mapM (`validateOverlapsForExams'` exams) overlaps

validateOverlapsForExams' :: Overlaps -> [Exam]
                          -> Writer [String] ValidationResult
validateOverlapsForExams' overlaps [] = return EverythingOk
validateOverlapsForExams' overlaps (exam:exams) = do
  examOk <- validateOverlapsForExam overlaps exam exams
  tailOk <- validateOverlapsForExams' overlaps exams
  return $ validationResult [examOk, tailOk]

validateOverlapsInAdjacentSlots :: Plan -> Writer [String] ValidationResult
validateOverlapsInAdjacentSlots plan = do
  tell ["### Checking overlaps in adjacent slots (hard)"]
  let maxDays  = (\x -> x-1) $ length $ examDays $ semesterConfig plan
      maxSlots = (\x -> x-1) $ length $ slotsPerDay $ semesterConfig plan
      daySlotPairs = [ (d,(i,j)) | d <- [0..maxDays]
                                 , (i,j) <- zip [0..] [1..maxSlots] ]
      examsList (d,i) =
        M.elems $ maybe M.empty examsInSlot $ M.lookup (d,i) $ slots plan
      exams = map (\(d,(i,j)) ->  (examsList (d,i), examsList (d,j)))
                  daySlotPairs
  validateOverlapsTwoLists (overlaps <$> constraints plan) exams

validateOverlapsSameDay :: Plan -> Writer [String] ValidationResult
validateOverlapsSameDay plan = do
  tell ["### Checking overlaps on same day (soft)"]
  let maxDays  = (\x -> x-1) $ length $ examDays $ semesterConfig plan
      maxSlots = (\x -> x-1) $ length $ slotsPerDay $ semesterConfig plan
      daySlotPairs = [ (d,(i,j)) | d <- [0..maxDays]
                                 , i <- [0..maxSlots - 2]
                                 , j <- [i+2..maxSlots]
                     ]
      examsList (d,i) =
        M.elems $ maybe M.empty examsInSlot $ M.lookup (d,i) $ slots plan
      exams = map (\(d,(i,j)) ->  (examsList (d,i), examsList (d,j)))
                  daySlotPairs
  valRes <- validateOverlapsTwoLists (overlaps <$> constraints plan) exams
  return $ case valRes of
    HardConstraintsBroken -> SoftConstraintsBroken
    _                     -> valRes

validateOverlapsTwoLists :: Maybe [Overlaps] -> [([Exam],[Exam])]
                 -> Writer [String] ValidationResult
validateOverlapsTwoLists Nothing _ = do
  tell ["#### no overlaps found"]
  return EverythingOk
validateOverlapsTwoLists (Just overlaps) exams =
  validationResult <$> mapM (validateOverlapsTwoListsForExams overlaps) exams

validateOverlapsTwoListsForExams :: [Overlaps] -> ([Exam],[Exam])
                                 -> Writer [String] ValidationResult
validateOverlapsTwoListsForExams overlaps (ex1, ex2) =
  validationResult <$> mapM (validateOverlapsTwoListsForExams' ex1 ex2) overlaps

validateOverlapsTwoListsForExams' :: [Exam] -> [Exam] -> Overlaps
                                  -> Writer [String] ValidationResult
validateOverlapsTwoListsForExams' [] _ _ = return EverythingOk
validateOverlapsTwoListsForExams' (exam:exams) otherExams overlaps = do
  examOk <- validateOverlapsForExam overlaps exam otherExams
  tailOk <- validateOverlapsTwoListsForExams' exams otherExams overlaps
  return $ validationResult [ examOk, tailOk ]

validateOverlapsForExam :: Overlaps -> Exam -> [Exam]
                        -> Writer [String] ValidationResult
validateOverlapsForExam _        _    []    = return EverythingOk
validateOverlapsForExam overlaps exam exams = do
  let overlapsForExam =
        M.findWithDefault M.empty (anCode exam) $ olOverlaps overlaps
      overlappingExams =
        filter (isJust . snd)
        $ map ((\ancode -> (ancode , M.lookup ancode overlapsForExam)) . anCode)
              exams
      ok = null overlappingExams
  forM_ overlappingExams $ \(other, Just studs) ->
    tell ["-   " ++ show (olGroup overlaps)
          ++ ": Exam " ++ show (anCode exam)
          ++ " overlaps with exam " ++ show other
          ++ " for " ++ show studs ++ " Students"]
  return $ if ok then EverythingOk else HardConstraintsBroken

--------------------------------------------------------------------------------
-- validate constraints from constraints file
--------------------------------------------------------------------------------
validateScheduleConstraints :: Plan -> Writer [String] Bool
validateScheduleConstraints plan = do
  tell ["## Validate schedule constraints from file"]
  let maybeConstraints = constraints plan
  case maybeConstraints of
    Nothing -> tell ["- no constraints found"] >> return True
    Just constraints -> do
      notOnSameDayOk <- validateNotOnSameDay plan (notOnSameDay constraints)
      onOneOfTheseDaysOk <- validateOneOfTheseDays plan
                                                  (onOneOfTheseDays constraints)
      fixedSlotOk <- validateFixSlot plan (fixedSlot constraints)
      tell ["TODO: invigilatesExam"]
      tell ["TODO: impossibleInvigilationSlots"]
      return $ notOnSameDayOk
            && onOneOfTheseDaysOk

------------------
-- not on same day
------------------
validateNotOnSameDay :: Plan -> [[Ancode]] -> Writer [String] Bool
validateNotOnSameDay plan listOfAncodes = do
  tell ["### Validate exams not on same day"]
  and <$> mapM (validateNotOnSameDay' plan) listOfAncodes

validateNotOnSameDay' :: Plan -> [Ancode] -> Writer [String] Bool
validateNotOnSameDay' plan ancodes = do
  let ancodesAndDays =
         filter (isJust . snd)
         $ map (\ancode -> ( ancode
                           , let exams = queryByAnCode ancode plan
                                 maybeDay = fst <$> slot (head exams)
                             in if null exams
                                then Nothing
                                else maybeDay
                           )
               ) ancodes
      ancodesAndDaysOk = (== length ancodesAndDays)
                         $ length $ nub $ map snd ancodesAndDays
  unless ancodesAndDaysOk $
    tell ["- not okay for " ++ show ancodesAndDays]
  return ancodesAndDaysOk

--------------------
-- one of these days
--------------------
validateOneOfTheseDays :: Plan -> [(Ancode, [Int])] -> Writer [String] Bool
validateOneOfTheseDays plan ancodesAndDays = do
  tell ["### Validate exams on one of these days"]
  and <$> mapM (validateOneOfTheseDays' plan) ancodesAndDays

validateOneOfTheseDays' :: Plan -> (Ancode, [Int]) -> Writer [String] Bool
validateOneOfTheseDays' plan (ancode, days) = do
  let exams = queryByAnCode ancode plan
      oneOfTheseDaysOk = case slot $ head exams of
        Nothing    -> True
        Just (d,_) -> d `elem` days
  if null exams
    then tell ["- exam " ++ show ancode ++ " not found"] >> return True
    else do
      unless oneOfTheseDaysOk $
        tell ["- exam " ++ show ancode ++ " not on one of " ++ show days]
      return oneOfTheseDaysOk

-------------
-- fixed slot
-------------
validateFixSlot :: Plan -> [(Ancode, (Int,Int))] -> Writer [String] Bool
validateFixSlot plan ancodesAndSlots = do
  tell ["### Validate exams in fixed slot"]
  and <$> mapM (validateFixSlot' plan) ancodesAndSlots

validateFixSlot' :: Plan -> (Ancode, (Int,Int)) -> Writer [String] Bool
validateFixSlot' plan (ancode, (d,s)) = do
  let exams = queryByAnCode ancode plan
      fixedSlotOk = case slot $ head exams of
        Nothing      -> True
        Just (d',s') -> d' == d && s' == s
  if null exams
    then tell ["- exam " ++ show ancode ++ " not found"] >> return True
    else do
      unless fixedSlotOk $
        tell ["- exam " ++ show ancode ++ " not in slot " ++ show (d,s)]
      return fixedSlotOk

-------------------------------------------------------
-- each lecturer should have zero or one exams per slot
-------------------------------------------------------

validateLecturerMaxOneExamPerSlot :: Plan -> Writer [String] ValidationResult
validateLecturerMaxOneExamPerSlot plan = do
  tell ["### Validate numbers of exams per lecturer per slot (soft)"]
  let listsOfLecturers = map (\(s,es) -> ( s
                                         , map lecturer
                                           $ M.elems
                                           $ examsInSlot es
                                         ))
                           $ M.toList $ slots plan
      slotsWithDuplicates = filter (((/=) <$> nub <*> id) . snd) listsOfLecturers
  forM_ slotsWithDuplicates $ \(slot, lecturers) ->
    tell ["-   ecturer has more then one exam in slot: "
          ++ show lecturers]
  return $ if null slotsWithDuplicates
           then EverythingOk
           else SoftConstraintsBroken

-------------------------------------------------
-- each student should have two exams max per day
-------------------------------------------------

-- TODO: Unterscheidung ob zwei Erstprüfungen an einem Tag oder Wiederholungs-
-- prüfungen
validateStudentsMaxTwoExamsPerDay :: Plan -> Writer [String] ValidationResult
validateStudentsMaxTwoExamsPerDay plan = do
  -- TODO: hard/soft okay?
  tell ["### Validate numbers of exams per student per day"
        ++" (2 are soft, 3 are hard, but not if noOfExams > 13)"]
  let examsPerDays :: [[Ancode]]
      examsPerDays = map (\d ->
                          concatMap (M.keys . examsInSlot)
                          $ mapMaybe (\s -> M.lookup (d,s) (slots plan))
                                      [0..maxSlotIndex plan])
                        [0..maxDayIndex plan]
      studentsWithMoreThanOneExamPerDay =
        map mkTuples $ groupWith head $ concatMap checkStudents examsPerDays
      checkStudents :: [Ancode] -> [[MtkNr]]
      checkStudents  = filter ((>1) . length)
                     . groupWith id
                     . concatMap (map fst . S.toList)
                     . mapMaybe (\a -> M.lookup a (students plan))
      mkTuples :: [[MtkNr]] -> (MtkNr, [Int])
      mkTuples mtknr = (head $ head mtknr, map length mtknr)
      oneHasThree = filter (\(mtknr, examsPerDay) -> any (>2) examsPerDay)
                            studentsWithMoreThanOneExamPerDay
      allAncodes = map anCode $ allExams plan
  forM_ studentsWithMoreThanOneExamPerDay $ \(mtknr, noOfExams) -> do
    let exams = maybe [] (filter (`elem` allAncodes) . S.toList . snd)
                          $ M.lookup mtknr $ studentsExams plan
    let three = any (>2) noOfExams
    tell [(if three then "-   Student has three or more exams a day:"
                    else "-   Student has two exams per day: ")
          ++ show noOfExams ++ " of " ++ show (length exams) ++ ", MtkNr "
          ++ show mtknr
          ++ "  \n" ++ show exams]

  return $ if null studentsWithMoreThanOneExamPerDay
           then EverythingOk
           else if null oneHasThree
                then SoftConstraintsBroken
                else let nosOfExams = map length
                           $ mapMaybe ((`M.lookup` studentsExams plan) . fst)
                                      oneHasThree
                     in if all (>13) nosOfExams
                        then SoftConstraintsBroken
                        else HardConstraintsBroken
