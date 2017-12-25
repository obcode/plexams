module Plexams.Types.Invigilation
  ( Invigilation(..)
  ) where

import Data.Maybe (fromMaybe)
import Plexams.Types.Common

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
