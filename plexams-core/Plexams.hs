module Plexams where

import           Plexams.Import
import           Plexams.PlanManip
import           Plexams.Types

initSemesterConfigFromFile :: FilePath -> IO (Maybe SemesterConfig)
initSemesterConfigFromFile = importSemesterConfigFromJSONFile

applyPlanManipToPlanWithFile :: Plan -> FilePath -> IO Plan
applyPlanManipToPlanWithFile plan filePath = do
    maybePlanManipList <- importPlanManipFromJSONFile filePath
    return $ maybe plan (applyPlanManipListToPlan plan) maybePlanManipList

