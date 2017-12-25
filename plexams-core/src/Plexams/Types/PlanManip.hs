{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.PlanManip
  ( AddExamToSlot(..)
  , AddRoomToExam(..)
  , AddInvigilatorToRoomOrSlot(..)
  ) where

import Control.Applicative (empty)
import Data.Aeson
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
  , addRoomStudentsInRoom :: [Integer]
  , addRoomDeltaDuration :: Maybe Integer
  } deriving (Show)

instance Y.FromJSON AddRoomToExam where
  parseJSON (Y.Object v) =
    AddRoomToExam <$> v Y..: "ancode" <*> v Y..: "room" <*>
    v Y..: "studentsInRoom" <*>
    v Y..:? "deltaDuration" Y..!= Nothing
  parseJSON _ = empty

data AddInvigilatorToRoomOrSlot = AddInvigilatorToRoomOrSlot
  { addInvigilatorID :: PersonID
  , addInvigilatorSlot :: (DayIndex, SlotIndex)
    -- Nothing means ReserveInvigilator for Slot
  , addInvigilatorRoom :: Maybe String
  }
