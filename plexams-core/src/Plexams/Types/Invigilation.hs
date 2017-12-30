{-# LANGUAGE DeriveGeneric #-}

module Plexams.Types.Invigilation
  ( Invigilation(..)
  , Invigilations(..)
  ) where

import Data.Maybe (fromMaybe)
import GHC.Generics

import Data.Aeson

import Plexams.Types.Common

data Invigilations = Invigilations
  { invigilationsSumExamRooms :: Integer
  , invigilationsSumReserve :: Integer
  , invigilationsSumOralExams :: Integer
  , invigilationsSumMaster :: Integer
  , invigilationsSumLivecoding :: Integer
  } deriving (Generic)

instance ToJSON Invigilations

data Invigilation = Invigilation
  { invigilationInvigilatorID :: PersonID
  , invigilationDay :: DayIndex
  , invigilationSlot :: SlotIndex
  , invigilationRoom :: Maybe RoomID -- Nothing means reserve for Slot
  , invigilationDuration :: Integer
  }

instance Show Invigilation where
  show (Invigilation _ d s mr dur) =
    "(" ++
    show d ++
    "," ++
    show s ++ ") " ++ fromMaybe "Reserve" mr ++ " " ++ show dur ++ " Minuten"
