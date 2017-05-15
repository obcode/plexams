module Plexams.Validation.Sources
  ( validate
  ) where

import           Control.Monad.Writer
import qualified Data.Map             as M
import           Plexams.Types

validate :: Plan -> Writer [String] Bool
validate plan = do
  tell ["## Validating Sources"]
  validateRegsAndOverlaps plan

-- Overlaps einer Prüfung mit sich selbst muss der Anmeldezahl
-- für diese Gruppe entsprechen
-- und das ganze muss symmetrisch sein.
validateRegsAndOverlaps :: Plan -> Writer [String] Bool
validateRegsAndOverlaps plan = do
  let maybeOverlaps = overlaps <$> constraints plan
  case maybeOverlaps of
    Nothing -> do
      tell ["### no constraints for plan defined"]
      return True
    Just overlaps ->
      and <$> mapM validateOverlapsForGroups overlaps

-- Überprüft die Symmetrie der Overlaps
validateOverlapsForGroups :: Overlaps -> Writer [String] Bool
validateOverlapsForGroups overlaps = do
  let group = show $ olGroup overlaps
      flatOverlaps = concatMap (\(a,m) ->
                            map (\(b,c) -> (a,b,c))
                         $ M.toList m)
                   $ M.toList $ olOverlaps overlaps
  tell ["### Checking integrity of overlaps file for " ++ group]
  and <$> mapM (findSymm group flatOverlaps) flatOverlaps

findSymm :: String -> [(Integer, Integer, Integer)]
         -> (Integer, Integer, Integer) -> Writer [String] Bool
findSymm group flatOverlaps o@(a,b,c) = do
  let found = (b,a,c) `elem` flatOverlaps
  unless found $
      tell ["- Overlaps for " ++ group
            ++ " are not symmetric " ++ show o ++ " "]
  return found
