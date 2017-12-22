{-# LANGUAGE OverloadedStrings #-}
module Plexams.Validation.Rooms
  ( validate
  ) where

import           Control.Monad.Writer
import           Data.List            (nub)
import qualified Data.Map             as M
import           Data.Text            (append)
import           Plexams.Types
import           TextShow             (showt)

validate :: Plan -> Writer [ValidationRecord] ValidationResult
validate plan = do
  tell [Info "## Validating Rooms"]
  -- normalRoomsNoHandicap
  -- handicapRoomsAllHandicap
  -- no student left outside of room
  enoughRoomsForExams <- validateEnoughRoomsForExams plan
  stillReserveForExams <- validationStillReserveForExams plan
  differentRoomsInSlot <- validateDifferentRoomsInSlots plan
  return $ validationResult
            [ enoughRoomsForExams
            , stillReserveForExams
            , differentRoomsInSlot
            ]

validateEnoughRoomsForExams :: Plan -> Writer [ValidationRecord] ValidationResult
validateEnoughRoomsForExams plan = do
  tell [Info "### Validating enough rooms for exam (hard)"]
  validationResult <$> mapM validateEnoughRoomsForExam
                            (filter plannedByMe $ scheduledExams plan)

validateEnoughRoomsForExam :: Exam -> Writer [ValidationRecord] ValidationResult
validateEnoughRoomsForExam exam = do
  let regs' = registrations exam
      seats = sum $ map maxSeats $ rooms exam
  when (regs'>seats) $
    tell [ HardConstraintBroken
         $ "- exam " `append` showt (anCode exam)
                     `append` " not enough rooms planned"]
  return $ if regs'<=seats
           then EverythingOk
           else HardConstraintsBroken

validationStillReserveForExams :: Plan -> Writer [ValidationRecord] ValidationResult
validationStillReserveForExams plan = do
  tell [Info "### Validating if there are at least 2 empty seats left for exam (soft)"]
  validationResult <$> mapM validationStillReserveForExam
                            (filter plannedByMe $ scheduledExams plan)

validationStillReserveForExam :: Exam -> Writer [ValidationRecord] ValidationResult
validationStillReserveForExam exam = do
  let regs' = registrations exam
      seats = sum $ map maxSeats $ rooms exam
  when (regs'+2>=seats) $
    tell [ SoftConstraintBroken
         $ "- exam " `append` showt (anCode exam)
           `append` " not enough reserve seats left: "
           `append` showt regs' `append`"/" `append` showt seats]
  return $ if regs'+2<=seats
           then EverythingOk
           else SoftConstraintsBroken

validateDifferentRoomsInSlots :: Plan -> Writer [ValidationRecord] ValidationResult
validateDifferentRoomsInSlots plan = do
  tell [Info "### Validating different rooms in slot (hard)"]
  validationResult <$> mapM validateDifferentRoomsInSlot (M.toList (slots plan))

validateDifferentRoomsInSlot :: ((DayIndex, SlotIndex), Slot)
                             -> Writer [ValidationRecord] ValidationResult
validateDifferentRoomsInSlot (index, slot') = do
  let exams' = examsInSlot slot'
      roomsDifferent rs =  length rs
                        == length (nub (map (roomID . fst) rs))
      plannedRooms = concatMap (\e -> map (\r -> (r, e)) $ rooms e) exams'
      allRoomsDifferent = roomsDifferent plannedRooms
      plannedRoomsWithoutReserveRooms =
                    filter (not . reserveRoom . fst) plannedRooms
      plannedRoomsWithoutReserveRoomsDifferent =
                    roomsDifferent plannedRoomsWithoutReserveRooms
      plannedRoomsWithoutHandicapCompensation =
                    filter (not . handicapCompensation . fst) plannedRooms
      plannedRoomsWithoutHandicapCompensationDifferent =
                    roomsDifferent plannedRoomsWithoutHandicapCompensation
  unless allRoomsDifferent $
    tell [ HardConstraintBroken
         $ "- slot "
           `append` showt index
           `append` ": same rooms used more than once"
         ]
  return $ if allRoomsDifferent
    then EverythingOk
    else if plannedRoomsWithoutReserveRoomsDifferent
            && plannedRoomsWithoutHandicapCompensationDifferent
         then SoftConstraintsBroken
         else HardConstraintsBroken
