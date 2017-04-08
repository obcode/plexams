module Plexams where

import           Data.List         (isSuffixOf)
import           Plexams.Import
import           Plexams.PlanManip
import           Plexams.Types

initSemesterConfigFromFile :: FilePath -> IO (Maybe SemesterConfig)
initSemesterConfigFromFile = importSemesterConfigFromYAMLFile

applyPlanManipToPlanWithFile :: Plan -> FilePath -> IO Plan
applyPlanManipToPlanWithFile plan filePath = do
    maybePlanManipList <-
        if ".json" `isSuffixOf` filePath
        then importPlanManipFromJSONFile filePath
        else if ".yaml" `isSuffixOf` filePath
             then importPlanManipFromYAMLFile filePath
             else return Nothing
    return $ maybe plan (applyPlanManipListToPlan plan) maybePlanManipList

