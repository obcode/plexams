module Plexams.Validation
    ( validatePlan
    ) where

import           Control.Arrow        ((&&&))
import           Control.Monad.Writer
import           Data.List            (nub)
import qualified Data.Map             as M
import           Data.Maybe           (mapMaybe)
import           GHC.Exts             (groupWith)
import           Plexams.Query
import           Plexams.Types

validatePlan :: Plan -> (Bool, [String])
validatePlan = runWriter . validatePlan'

validatePlan' :: Plan -> Writer [String] Bool
validatePlan' plan = do
  goSlotsOk <- validateGOSlots plan
  regsAndOverlapsOK <- validateRegsAndOverlaps plan
  lecturersMax3ExamDays <- validateLecturersMax3ExamDays plan
  sameNameSameSlot <- validateSameNameSameSlot plan
  tell ["# no more validations implemented yet"]
  return $ goSlotsOk
        && regsAndOverlapsOK
        && lecturersMax3ExamDays
        && sameNameSameSlot
        && False

validateGOSlots :: Plan -> Writer [String] Bool
validateGOSlots plan = do
  tell ["# Checking GO-Slots!"]
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

-- Overlaps einer Prüfung mit sich selbst muss der Anmeldezahl
-- für diese Gruppe entsprechen
-- und das ganze muss symmetrisch sein.
validateRegsAndOverlaps :: Plan -> Writer [String] Bool
validateRegsAndOverlaps plan = do
  let maybeOverlaps = overlaps <$> constraints plan
  case maybeOverlaps of
    Nothing -> do
      tell ["# no constraints for plan defined"]
      return True
    Just overlaps ->
      and <$> mapM validateOverlapsForGroup overlaps

-- Überprüft die Symmetrie der Overlaps
validateOverlapsForGroup :: Overlaps -> Writer [String] Bool
validateOverlapsForGroup overlaps = do
  let group = show $ olGroup overlaps
      flatOverlaps = concatMap (\(a,m) ->
                            map (\(b,c) -> (a,b,c))
                         $ M.toList m)
                   $ M.toList $ olOverlaps overlaps
  tell ["# Checking overlaps for " ++ group]
  and <$> mapM (findSymm group flatOverlaps) flatOverlaps

findSymm :: String -> [(Integer, Integer, Integer)]
         -> (Integer, Integer, Integer) -> Writer [String] Bool
findSymm group flatOverlaps o@(a,b,c) = do
  let found = (b,a,c) `elem` flatOverlaps
  unless found $
      tell ["- Overlaps for " ++ group
            ++ " are not symmetric " ++ show o ++ " "]
  return found

validateLecturersMax3ExamDays :: Plan -> Writer [String] Bool
validateLecturersMax3ExamDays plan = do
  let lecturerWithMoreThan3ExamDays =
          filter ((>3) . length . snd) $ lecturerExamDays plan
      ok = null lecturerWithMoreThan3ExamDays
  tell ["# Checking amount of exam days for each lecturer"]
  unless ok $
    mapM_ (\(l,d) ->
              tell ["- More than 3 days of exams: "
                    ++ personShortName l
                    ++ ": " ++ show d])
          lecturerWithMoreThan3ExamDays
  return ok

validateSameNameSameSlot :: Plan -> Writer [String] Bool
validateSameNameSameSlot plan = do
  let examsGroupedByNameWithDifferentSlots =
            filter ((>1) . length . nub . map slot)
            $ groupWith name
            $ allExams plan
      ok = null examsGroupedByNameWithDifferentSlots
  tell ["# Checking exams with same name in same slot"]
  unless ok $
    mapM_ ( tell
            . (:[])
            . ("- exams with same name but different slots: "++)
            . show
            . map (anCode &&& slot)
          )
          examsGroupedByNameWithDifferentSlots
  return ok
