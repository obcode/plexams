module Plexams.Types.Invigilation
    ( Invigilation(..)
    ) where

import           Plexams.Types.Common

data Invigilation =
  Invigilation
    { invigilationInvigilatorID :: PersonID
    , invigilationDay           :: DayIndex
    , invigilationSlot          :: SlotIndex
    , invigilationRoom          :: Maybe RoomID -- Nothing means reserve for Slot
    , invigilationDuration      :: Integer
    }
