{-# LANGUAGE OverloadedStrings #-}
module Plexams.Types.PlanManip
  ( AddExamToSlot(..)
  , AddRoomToExam(..)
  , AddInvigilatorToRoomOrSlot(..)
  ) where

import           Control.Applicative  (empty)
import qualified Data.Yaml            as Y
import           Plexams.Types.Common

data AddExamToSlot =
    AddExamToSlot
      { planManipAnCode :: Integer
      , planManipDay    :: Int
      , planManipSlot   :: Int
      }

data AddRoomToExam =
    AddRoomToExam
      { addRoomAnCode        :: Integer
      , addRoomRoomName      :: String
      , addRoomSeatsPlanned  :: Integer
      , addRoomDeltaDuration :: Maybe Integer
      }
  deriving Show

instance Y.FromJSON AddRoomToExam where
    parseJSON (Y.Object v) = AddRoomToExam
                       <$> v Y..: "ancode"
                       <*> v Y..: "room"
                       <*> v Y..: "seatsPlanned"
                       <*> v Y..:? "deltaDuration" Y..!= Nothing
    parseJSON _            = empty

data AddInvigilatorToRoomOrSlot =
  AddInvigilatorToRoomOrSlot
    { addInvigilatorID   :: PersonID
    , addInvigilatorSlot :: (DayIndex, SlotIndex)
    -- Nothing means ReserveInvigilator for Slot
    , addInvigilatorRoom :: Maybe String
    }
