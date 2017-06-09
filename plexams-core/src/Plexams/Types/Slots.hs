module Plexams.Types.Slots
  ( Slot(..)
  , Slots
  ) where

import qualified Data.Map             as M
import           Plexams.Types.Common
import           Plexams.Types.Exam

type Slots = M.Map (DayIndex, SlotIndex) Slot

data Slot = Slot
    { examsInSlot        :: M.Map Ancode Exam -- Ancode -> Exam
    , reserveInvigilator :: Maybe Integer  -- ^ Reserveaufsicht für die Prüfung
    }
  deriving (Show, Eq)
