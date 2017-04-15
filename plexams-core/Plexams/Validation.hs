module Plexams.Validation
    ( validatePlan
    ) where

import           Control.Monad.Writer
import qualified Data.Map             as M
import           Data.Maybe           (mapMaybe)
import           Plexams.Types

validatePlan :: Plan -> (Bool, [String])
validatePlan = runWriter . validatePlan'

validatePlan' :: Plan -> Writer [String] Bool
validatePlan' plan = do
  goSlotsOk <- validateGOSlots plan
  regsAndOverlapsOK <- validateRegsAndOverlaps plan
  tell ["# no more validations implemented yet"]
  return $ goSlotsOk && regsAndOverlapsOK && False

validateGOSlots :: Plan -> Writer [String] Bool
validateGOSlots plan = do
  tell ["# Checking GO-Slots!"]
  let allowedSlots = goSlots $ semesterConfig plan
      examsInOtherSlots = filter (elem GO . map groupDegree . groups)
                        $ concatMap examsInSlot
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
  overlapsForGroupOk <- case maybeOverlaps of
    Nothing -> do
      tell ["# no constraints for plan defined"]
      return True
    Just overlaps ->
      and <$> mapM validateOverlapsForGroup overlaps
  return overlapsForGroupOk

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
      tell ["Overlaps for " ++ group
            ++ " are not symmetric " ++ show o ++ " "]
  return found
