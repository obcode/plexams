{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.PlanManip
  ( AddExamToSlot(..)
  , AddRoomToExam(..)
  , AddInvigilatorToRoomOrSlot(..)
  , RemoveInvigilatorFromRoomOrSlot(..)
  ) where

import Control.Applicative (empty)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Yaml as Y
import GHC.Generics

import Plexams.Types.Common

data AddExamToSlot = AddExamToSlot
  { planManipAnCode :: Integer
  , planManipDay :: Int
  , planManipSlot :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON AddExamToSlot

instance FromJSON AddExamToSlot

data AddRoomToExam = AddRoomToExam
  { addRoomAnCode :: Integer
  , addRoomRoomName :: String
  , addRoomStudentsInRoom :: [Text]
  , addRoomDeltaDuration :: Maybe Integer
  , addRoomNTA :: Bool
  , addRoomReserve :: Bool
  } deriving (Show)

instance Y.FromJSON AddRoomToExam where
  parseJSON (Y.Object v) =
    AddRoomToExam <$> v Y..: "ancode" <*> v Y..: "room" <*>
    v Y..: "studentsInRoom" <*>
    v Y..:? "deltaDuration" Y..!= Nothing <*>
    v Y..:? "nta" Y..!= False <*>
    v Y..:? "reserve" Y..!= False
  parseJSON _ = empty

data AddInvigilatorToRoomOrSlot = AddInvigilatorToRoomOrSlot
  { addInvigilatorID :: PersonID
  , addInvigilatorSlot :: (DayIndex, SlotIndex)
    -- Nothing means ReserveInvigilator for Slot
  , addInvigilatorRoom :: Maybe String
  } deriving (Eq, Show, Generic)

instance Y.ToJSON AddInvigilatorToRoomOrSlot where
  toJSON (AddInvigilatorToRoomOrSlot i s mr) =
    object ["id" .= i, "slot" .= s, "room" .= mr]

instance FromJSON AddInvigilatorToRoomOrSlot

data RemoveInvigilatorFromRoomOrSlot = RemoveInvigilatorFromRoomOrSlot
  { removeInvigilatorID :: PersonID
  , removeInvigilatorSlot :: (DayIndex, SlotIndex)
    -- Nothing means ReserveInvigilator for Slot
  , removeInvigilatorRoom :: Maybe String
  } deriving (Eq, Show, Generic)

instance ToJSON RemoveInvigilatorFromRoomOrSlot

instance FromJSON RemoveInvigilatorFromRoomOrSlot
