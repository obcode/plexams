module Plexams.Types.Constraints
  ( Constraints(..)
  , noConstraints
  , Overlaps(..)
  ) where

import qualified Data.Map             as M
import           Plexams.Types.Common
import           Plexams.Types.Groups

data Constraints = Constraints
  { overlaps                    :: [Overlaps]
  , notOnSameDay                :: [[Ancode]]
  , onOneOfTheseDays            :: [(Ancode, [Int])]
  , fixedSlot                   :: [(Ancode, (Int,Int))]
  , invigilatesExam             :: [(Ancode, PersonID)]
  , impossibleInvigilationSlots :: [(PersonID, [(Int, Int)])]
  , roomSlots                   :: M.Map RoomID [(DayIndex, SlotIndex)]
  }
  deriving (Show, Eq)

noConstraints :: Constraints
noConstraints = Constraints [] [] [] [] [] [] M.empty

data Overlaps = Overlaps
  { olGroup :: Group
  , olOverlaps :: M.Map Integer    -- ancode
                        (M.Map Integer -- otherAncode
                               Integer -- noOfStudents
                         )
  }
  deriving (Show, Eq)
