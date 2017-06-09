{-# LANGUAGE OverloadedStrings #-}
module Plexams.Types.PlanManip
  ( AddExamToSlot(..)
  , AddRoomToExam(..)
  ) where

import           Control.Applicative (empty)
import qualified Data.Yaml           as Y

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
