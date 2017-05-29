{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import.PlanManip
    ( importExamSlotsFromYAMLFile
    ) where

import qualified Data.ByteString as BSI
import qualified Data.Yaml       as Y
import           Plexams.Types

--------------------------------------------------------------------------------
-- AddExamToSlot from YAML file
--------------------------------------------------------------------------------

listsToExamSlots :: Maybe [[Integer]] -> Maybe [AddExamToSlot]
listsToExamSlots = fmap $ map listToExamSlots

listToExamSlots :: [Integer] -> AddExamToSlot
listToExamSlots [a,d,s] = AddExamToSlot a (fromInteger d) (fromInteger s)
listToExamSlots xs      = error $ "cannot decode " ++ show xs

importExamSlotsFromYAMLFile :: FilePath -> IO (Maybe [AddExamToSlot])
importExamSlotsFromYAMLFile = fmap (listsToExamSlots . Y.decode) . BSI.readFile
