{-# LANGUAGE OverloadedStrings #-}
module Plexams.Validation.Rooms
  ( validate
  ) where

import           Control.Arrow        ((&&&))
import           Control.Monad.Writer
import           Data.List            (nub)
import qualified Data.Map             as M
import           Data.Maybe           (isJust, mapMaybe)
import qualified Data.Set             as S
import           Data.Text            (Text, append)
import           GHC.Exts             (groupWith)
import           Plexams.Query
import           Plexams.Types
import           TextShow             (showt)

validate :: Plan -> Writer [Text] ValidationResult
validate plan = do
  tell ["## Validating Rooms"]
  enoughRoomsForExams <- validateEnoughRoomsForExams plan
  stillReserveForExams <- validationStillReserveForExams plan
  differentRoomsInSlot <- validateDifferentRoomsInSlots plan
  return $ validationResult
            [ enoughRoomsForExams
            , stillReserveForExams
            , differentRoomsInSlot
            ]

validateEnoughRoomsForExams :: Plan -> Writer [Text] ValidationResult
validateEnoughRoomsForExams plan = do
  tell ["### Validating enough rooms for exam (hard)"]
  validationResult <$> mapM validateEnoughRoomsForExam
                            (filter plannedByMe $ scheduledExams plan)

validateEnoughRoomsForExam :: Exam -> Writer [Text] ValidationResult
validateEnoughRoomsForExam exam = do
  let regs = registrations exam
      seats = sum $ map maxSeats $ rooms exam
  when (regs>seats) $
    tell ["- exam " `append` showt (anCode exam)
                    `append` " not enough rooms planned"]
  return $ if regs<=seats
           then EverythingOk
           else HardConstraintsBroken

validationStillReserveForExams :: Plan -> Writer [Text] ValidationResult
validationStillReserveForExams plan = do
  tell ["### Validating if there are at least 2 empty seats left for exam (soft)"]
  validationResult <$> mapM validationStillReserveForExam
                            (filter plannedByMe $ scheduledExams plan)

validationStillReserveForExam :: Exam -> Writer [Text] ValidationResult
validationStillReserveForExam exam = do
  let regs = registrations exam
      seats = sum $ map maxSeats $ rooms exam
  when (regs+2>=seats) $
    tell ["- exam " `append` showt (anCode exam)
          `append` " not enough reserve seats left: "
          `append` showt regs `append`"/" `append` showt seats]
  return $ if regs+2<=seats
           then EverythingOk
           else SoftConstraintsBroken

validateDifferentRoomsInSlots :: Plan -> Writer [Text] ValidationResult
validateDifferentRoomsInSlots plan = do
  tell ["### Validating different rooms in slot (hard)"]
  validationResult <$> mapM validateDifferentRoomsInSlot (M.elems (slots plan))

validateDifferentRoomsInSlot :: Slot -> Writer [Text] ValidationResult
validateDifferentRoomsInSlot slot = do
  tell ["TODO: fixme"]
  return HardConstraintsBroken
