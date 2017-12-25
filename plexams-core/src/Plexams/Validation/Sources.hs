{-# LANGUAGE OverloadedStrings #-}

module Plexams.Validation.Sources
  ( validate
  ) where

import Control.Monad.Writer
import qualified Data.Map as M
import Data.Text (Text, append)
import Plexams.Types
import TextShow

validate :: Plan -> Writer [ValidationRecord] ValidationResult
validate plan = do
  tell [Info "## Validating Sources"]
  validateRegsAndOverlaps plan

-- Overlaps einer Prüfung mit sich selbst muss der Anmeldezahl
-- für diese Gruppe entsprechen
-- und das ganze muss symmetrisch sein.
validateRegsAndOverlaps :: Plan -> Writer [ValidationRecord] ValidationResult
validateRegsAndOverlaps plan = do
  let overlaps' = overlaps $ constraints plan
  validationResult <$> mapM validateOverlapsForGroups overlaps'

-- Überprüft die Symmetrie der Overlaps
validateOverlapsForGroups ::
     Overlaps -> Writer [ValidationRecord] ValidationResult
validateOverlapsForGroups overlaps' = do
  let group = showt $ olGroup overlaps'
      flatOverlaps =
        concatMap (\(a, m) -> map (\(b, c) -> (a, b, c)) $ M.toList m) $
        M.toList $ olOverlaps overlaps'
  tell
    [ Info $
      "### Checking integrity of overlaps file for " `append` group `append`
      " (hard)"
    ]
  validationResult <$> mapM (findSymm group flatOverlaps) flatOverlaps

findSymm ::
     Text
  -> [(Integer, Integer, Integer)]
  -> (Integer, Integer, Integer)
  -> Writer [ValidationRecord] ValidationResult
findSymm group flatOverlaps o@(a, b, c) = do
  let found = (b, a, c) `elem` flatOverlaps
  unless found $
    tell
      [ HardConstraintBroken $
        "- Overlaps for " `append` group `append` " are not symmetric " `append`
        showt o `append`
        " "
      ]
  return $
    if found
      then EverythingOk
      else HardConstraintsBroken
