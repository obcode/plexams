{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Plexams.Types.Slots
  ( Slot(..)
  , Slots
  , adjacentSlotPairs
  , slotsByDay
  ) where

import           Data.Aeson
import qualified Data.Map              as M
import           Data.Maybe            (mapMaybe)
import           GHC.Exts              (groupWith)
import           GHC.Generics
import           Plexams.Types.Common
import           Plexams.Types.Exam
import           Plexams.Types.Persons

type Slots = M.Map (DayIndex, SlotIndex) Slot

data Slot = Slot
    { examsInSlot        :: M.Map Ancode Exam -- Ancode -> Exam
    , reserveInvigilator :: Maybe Invigilator  -- ^ Reserveaufsicht für die Prüfung
    }
  deriving (Show, Eq, Generic)

instance FromJSON Slot
instance ToJSON Slot

-- dayindex: from 0 to maxDayIndex
-- slotindex: from 0 to maxSlotIndex
adjacentSlotPairs :: Slots -> [[((DayIndex, SlotIndex), Slot)]]
adjacentSlotPairs slots =
  let maxSlotIndex = maximum $ map snd $ M.keys slots
      maxDayIndex = maximum $ map fst $ M.keys slots
      indexPairs = map (\(d,s) -> map (d,) s)
                       [(d, slotIndexPair)
                       | d <- [0..maxDayIndex]
                       , slotIndexPair <- [[x,x+1] | x <- [0..maxSlotIndex-1]]
                       ]
      maybeSlotPair [idx1, idx2] = do
        slot1 <- M.lookup idx1 slots
        slot2 <- M.lookup idx2 slots
        return [(idx1, slot1), (idx2, slot2)]
      maybeSlotPair _ = Nothing
  in mapMaybe maybeSlotPair indexPairs

slotsByDay :: Slots -> [[((DayIndex, SlotIndex), Slot)]]
slotsByDay slots = groupWith (fst . fst) $ M.toList slots
