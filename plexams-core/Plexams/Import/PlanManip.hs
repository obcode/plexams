{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import.PlanManip
    ( importExamSlotsFromYAMLFile
    , importAddRoomToExamFromYAMLFile
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import qualified Data.ByteString     as BSI
import qualified Data.Yaml           as Y
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

--------------------------------------------------------------------------------
-- AddRoomToExam from YAML file
--------------------------------------------------------------------------------

instance Y.FromJSON AddRoomToExam where
    parseJSON (Y.Object v) = AddRoomToExam
                       <$> v Y..: "ancode"
                       <*> v Y..: "room"
                       <*> v Y..: "seatsPlanned"
                       <*> v Y..:? "deltaDuration" Y..!= Nothing
    parseJSON _            = empty

listsToAddRoomToExam :: Maybe [[String]] -> Maybe [AddRoomToExam]
listsToAddRoomToExam = fmap $ map listToAddRoomToExam

listToAddRoomToExam :: [String] -> AddRoomToExam
listToAddRoomToExam [a,r,s,dd] = AddRoomToExam (read a) r (read s)
                                                                (Just $ read dd)
listToAddRoomToExam [a,r,s]    = AddRoomToExam (read a) r (read s) Nothing
listToAddRoomToExam xs         = error $ "cannot decode " ++ show xs

importAddRoomToExamFromYAMLFile :: FilePath -> IO (Maybe [AddRoomToExam])
importAddRoomToExamFromYAMLFile =
  fmap Y.decode . BSI.readFile
