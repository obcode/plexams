{-# LANGUAGE DeriveGeneric #-}

module Plexams.Types.Constraints
  ( Constraints(..)
  , noConstraints
  , Overlaps(..)
  ) where

import           Data.Aeson
import qualified Data.Map             as M
import           GHC.Generics
import           Plexams.Types.Common
import           Plexams.Types.Groups

data Constraints = Constraints
  { overlaps                    :: [Overlaps]
  , notOnSameDay                :: [[Ancode]]
  , inSameSlot                  :: [[Ancode]]
  , onOneOfTheseDays            :: [(Ancode, [Int])]
  , fixedSlot                   :: [(Ancode, (Int,Int))]
  , noInvigilations             :: [PersonID]
  , noInvigilationDays          :: [(PersonID, [DayIndex])]
  , invigilatesExam             :: [(Ancode, PersonID)]
  , impossibleInvigilationSlots :: [(PersonID, [(Int, Int)])]
  , roomSlots                   :: M.Map RoomID [(DayIndex, SlotIndex)]
  }
  deriving (Show, Eq)

noConstraints :: Constraints
noConstraints = Constraints [] [] [] [] [] [] [] [] [] M.empty

data Overlaps = Overlaps
  { olGroup :: Group
  , olOverlaps :: M.Map Integer    -- ancode
                        (M.Map Integer -- otherAncode
                               Integer -- noOfStudents
                         )
  }
  deriving (Show, Eq, Generic)

instance ToJSON Overlaps
