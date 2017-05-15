module Plexams.Validation.ScheduledExams
  ( validate
  ) where

import           Control.Arrow        ((&&&))
import           Control.Monad.Writer
import           Data.List            (nub)
import qualified Data.Map             as M
import           Data.Maybe           (isJust, mapMaybe)
import           GHC.Exts             (groupWith)
import           Plexams.Query
import           Plexams.Types

validate :: Plan -> Writer [String] Bool
validate plan = do
  constraintsOk <- validateScheduleConstraints plan
  tell ["## Validating Schedule"]
  goSlotsOk <- validateGOSlots plan
  sameNameSameSlot <- validateSameNameSameSlot plan
  sameSlotOverlaps <- validateOverlapsInSameSlot plan
  adjacentSlotOverlaps <- validateOverlapsInAdjacentSlots plan
  sameDayOverlaps <- validateOverlapsSameDay plan
  return $ goSlotsOk
        && sameNameSameSlot
        && sameSlotOverlaps
        && adjacentSlotOverlaps

validateGOSlots :: Plan -> Writer [String] Bool
validateGOSlots plan = do
  tell ["### Checking GO-Slots!"]
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
  return goOk

validateSameNameSameSlot :: Plan -> Writer [String] Bool
validateSameNameSameSlot plan = do
  let examsGroupedByNameWithDifferentSlots =
            filter ((>1) . length . nub . map slot)
            $ groupWith name
            $ allExams plan
      ok = null examsGroupedByNameWithDifferentSlots
  tell ["### Checking exams with same name in same slot"]
  unless ok $
    mapM_ ( tell
            . (:[])
            . ("- exams with same name but different slots: "++)
            . show
            . map (anCode &&& slot)
          )
          examsGroupedByNameWithDifferentSlots
  return ok

validateOverlapsInSameSlot :: Plan -> Writer [String] Bool
validateOverlapsInSameSlot plan = do
  tell ["### Checking overlaps in same slot"]
  validateOverlaps (overlaps <$> constraints plan)
                   (map (M.elems . examsInSlot) $ M.elems $ slots plan)

validateOverlaps :: Maybe [Overlaps] -> [[Exam]] -> Writer [String] Bool
validateOverlaps Nothing _ = tell ["#### no overlaps found"] >> return True
validateOverlaps (Just overlaps) exams =
  and <$> mapM (validateOverlapsForExams overlaps) exams

validateOverlapsForExams :: [Overlaps] -> [Exam] -> Writer [String] Bool
validateOverlapsForExams overlaps exams =
  and <$> mapM (`validateOverlapsForExams'` exams) overlaps

validateOverlapsForExams' :: Overlaps -> [Exam] -> Writer [String] Bool
validateOverlapsForExams' overlaps [] = return True
validateOverlapsForExams' overlaps (exam:exams) = do
  examOk <- validateOverlapsForExam overlaps exam exams
  tailOk <- validateOverlapsForExams' overlaps exams
  return $ examOk && tailOk

validateOverlapsInAdjacentSlots :: Plan -> Writer [String] Bool
validateOverlapsInAdjacentSlots plan = do
  tell ["### Checking overlaps in adjacent slots"]
  let maxDays  = (\x -> x-1) $ length $ examDays $ semesterConfig plan
      maxSlots = (\x -> x-1) $ length $ slotsPerDay $ semesterConfig plan
      daySlotPairs = [ (d,(i,j)) | d <- [0..maxDays]
                                 , (i,j) <- zip [0..] [1..maxSlots] ]
      examsList (d,i) =
        M.elems $ maybe M.empty examsInSlot $ M.lookup (d,i) $ slots plan
      exams = map (\(d,(i,j)) ->  (examsList (d,i), examsList (d,j)))
                  daySlotPairs
  validateOverlapsTwoLists (overlaps <$> constraints plan) exams

validateOverlapsSameDay :: Plan -> Writer [String] Bool
validateOverlapsSameDay plan = do
  tell ["### Checking overlaps on same day"]
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
  validateOverlapsTwoLists (overlaps <$> constraints plan) exams

validateOverlapsTwoLists :: Maybe [Overlaps] -> [([Exam],[Exam])]
                 -> Writer [String] Bool
validateOverlapsTwoLists Nothing _ = tell ["#### no overlaps found"] >> return True
validateOverlapsTwoLists (Just overlaps) exams =
  and <$> mapM (validateOverlapsTwoListsForExams overlaps) exams

validateOverlapsTwoListsForExams :: [Overlaps] -> ([Exam],[Exam])
                                 -> Writer [String] Bool
validateOverlapsTwoListsForExams overlaps (ex1, ex2) =
  and <$> mapM (validateOverlapsTwoListsForExams' ex1 ex2) overlaps

validateOverlapsTwoListsForExams' :: [Exam] -> [Exam] -> Overlaps
                                  -> Writer [String] Bool
validateOverlapsTwoListsForExams' [] _ _ = return True
validateOverlapsTwoListsForExams' (exam:exams) otherExams overlaps = do
  examOk <- validateOverlapsForExam overlaps exam otherExams
  tailOk <- validateOverlapsTwoListsForExams' exams otherExams overlaps
  return $ examOk && tailOk

validateOverlapsForExam :: Overlaps -> Exam -> [Exam] -> Writer [String] Bool
validateOverlapsForExam _        _    []    = return True
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
  return ok

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
