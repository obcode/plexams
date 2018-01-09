{-# LANGUAGE OverloadedStrings #-}

module Plexams.Import.PlanManip
  ( importExamSlotsFromYAMLFile
  , importAddRoomToExamFromYAMLFile
  , importAddInvigilatorToRoomOrSlotFromYAMLFile
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import qualified Data.ByteString as BSI
import qualified Data.Yaml as Y

import Plexams.Types

--------------------------------------------------------------------------------
-- AddExamToSlot from YAML file
--------------------------------------------------------------------------------
listsToExamSlots :: Maybe [[Integer]] -> Maybe [AddExamToSlot]
listsToExamSlots = fmap $ map listToExamSlots

listToExamSlots :: [Integer] -> AddExamToSlot
listToExamSlots [a, d, s] = AddExamToSlot a (fromInteger d) (fromInteger s)
listToExamSlots xs = error $ "cannot decode " ++ show xs

importExamSlotsFromYAMLFile :: FilePath -> IO (Maybe [AddExamToSlot])
importExamSlotsFromYAMLFile = fmap (listsToExamSlots . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- AddRoomToExam from YAML file
--------------------------------------------------------------------------------
importAddRoomToExamFromYAMLFile :: FilePath -> IO (Maybe [AddRoomToExam])
importAddRoomToExamFromYAMLFile = fmap Y.decode . BSI.readFile

--------------------------------------------------------------------------------
-- AddInvigilatorToRoomOrSlot from YAML file
--------------------------------------------------------------------------------
data ImportAddInvigilator =
  ImportAddInvigilator Integer
                       [Int]
                       (Maybe String)
  deriving (Show)

instance Y.FromJSON ImportAddInvigilator where
  parseJSON (Y.Object v) =
    ImportAddInvigilator <$> v Y..: "id" <*> v Y..: "slot" <*> v Y..:? "room"
  parseJSON _ = empty

iAI2AI :: ImportAddInvigilator -> AddInvigilatorToRoomOrSlot
iAI2AI (ImportAddInvigilator p [d, s] r) =
  AddInvigilatorToRoomOrSlot
  {addInvigilatorID = p, addInvigilatorSlot = (d, s), addInvigilatorRoom = r}
iAI2AI ia = error $ show ia

importAddInvigilatorToRoomOrSlotFromYAMLFile ::
     FilePath -> IO (Maybe [AddInvigilatorToRoomOrSlot])
importAddInvigilatorToRoomOrSlotFromYAMLFile =
  fmap (fmap (map iAI2AI) . Y.decode) . BSI.readFile
