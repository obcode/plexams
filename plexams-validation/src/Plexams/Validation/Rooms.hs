{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plexams.Validation.Rooms
  ( validate
  )
where

import           Control.Arrow                  ( second )
import           Control.Monad.Writer
import           Data.List                      ( nub )
import qualified Data.Map                      as M
import           Data.Text                      ( append )

import           TextShow                       ( showt )

import           Plexams.Types

validate :: Plan -> Writer [ValidationRecord] ValidationResult
validate plan = do
  tell [Info "## Validating Rooms"]
  -- normalRoomsNoHandicap
  -- handicapRoomsAllHandicap
  -- no student left outside of room
  enoughRoomsForExams      <- validateEnoughRoomsForExams plan
  stillReserveForExams     <- validationStillReserveForExams plan
  differentRoomsInSlot     <- validateDifferentRoomsInSlots plan
  noStudentLeftOutsideRoom <- validateNoStudentLeftOutsideRoom plan
  allRoomsAllowedInSlots   <- validateRoomsAllowedInSlots plan
  -- TODO: roomSlots eingehalten, inSameRoom eingehalten?
  return $ validationResult
    [ enoughRoomsForExams
    , stillReserveForExams
    , differentRoomsInSlot
    , noStudentLeftOutsideRoom
    , allRoomsAllowedInSlots
    ]

validateNoStudentLeftOutsideRoom
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateNoStudentLeftOutsideRoom plan = do
  tell [Info "### Validation that all students have a seat"]
  validationResult <$> mapM validateNoStudentLeftOutsideRoomForExam
                            (filter plannedByMe $ scheduledExams plan)

validateNoStudentLeftOutsideRoomForExam
  :: Exam -> Writer [ValidationRecord] ValidationResult
validateNoStudentLeftOutsideRoomForExam exam =
  if null $ registeredStudents exam
    then return EverythingOk
    else do
      tell
        [ HardConstraintBroken
          $        "- exam "
          `append` showt (anCode exam)
          `append` ": no room for "
          `append` showt (map studentName $ registeredStudents exam)
        ]
      return HardConstraintsBroken

validateEnoughRoomsForExams
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateEnoughRoomsForExams plan = do
  tell [Info "### Validating enough rooms for exam (hard)"]
  validationResult <$> mapM validateEnoughRoomsForExam
                            (filter plannedByMe $ scheduledExams plan)

validateEnoughRoomsForExam :: Exam -> Writer [ValidationRecord] ValidationResult
validateEnoughRoomsForExam exam = do
  let regs' = registrations exam
      seats = sum $ map maxSeats $ rooms exam
  if regs' > seats
    then do
      tell
        [ HardConstraintBroken
          $        "- exam "
          `append` showt (anCode exam)
          `append` " not enough rooms planned"
        ]
      return HardConstraintsBroken
    else return EverythingOk

validationStillReserveForExams
  :: Plan -> Writer [ValidationRecord] ValidationResult
validationStillReserveForExams plan = do
  tell
    [ Info
        "### Validating if there are at least 2 empty seats left for exam (soft)"
    ]
  validationResult <$> mapM validationStillReserveForExam
                            (filter plannedByMe $ scheduledExams plan)

validationStillReserveForExam
  :: Exam -> Writer [ValidationRecord] ValidationResult
validationStillReserveForExam exam = do
  let regs' = registrations exam
      seats = sum $ map maxSeats $ rooms exam
  if regs' + 2 >= seats
    then do
      tell
        [ SoftConstraintBroken
          $        "- exam "
          `append` showt (anCode exam)
          `append` " not enough reserve seats left: "
          `append` showt regs'
          `append` "/"
          `append` showt seats
        ]
      return SoftConstraintsBroken
    else return EverythingOk

-- TODO: sameRoom, ntainnormalroom
validateDifferentRoomsInSlots
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateDifferentRoomsInSlots plan = do
  tell [Info "### Validating different rooms in slot (hard)"]
  validationResult <$> mapM validateDifferentRoomsInSlot (M.toList (slots plan))

validateDifferentRoomsInSlot
  :: ((DayIndex, SlotIndex), Slot) -> Writer [ValidationRecord] ValidationResult
validateDifferentRoomsInSlot (index, slot') = do
  let
    exams' = examsInSlot slot'
    roomsDifferent rs = length rs == length (nub (map (roomID . fst) rs))
    plannedRooms      = concatMap (\e -> map (, e) $ rooms e) exams'
    allRoomsDifferent = roomsDifferent plannedRooms
    plannedRoomsWithoutReserveRooms =
      filter (not . reserveRoom . fst) plannedRooms
    plannedRoomsWithoutReserveRoomsDifferent =
      roomsDifferent plannedRoomsWithoutReserveRooms
    plannedRoomsWithoutHandicapCompensation =
      filter (not . handicapCompensation . fst) plannedRooms
    plannedRoomsWithoutHandicapCompensationDifferent =
      roomsDifferent plannedRoomsWithoutHandicapCompensation
  unless allRoomsDifferent $ tell
    [ HardConstraintBroken
      $        "- slot "
      `append` showt index
      `append` ": same rooms used more than once"
    ]
  return $ if allRoomsDifferent
    then EverythingOk
    else
      if plannedRoomsWithoutReserveRoomsDifferent
           && plannedRoomsWithoutHandicapCompensationDifferent
        then SoftConstraintsBroken
        else HardConstraintsBroken

validateRoomsAllowedInSlots
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateRoomsAllowedInSlots plan = do
  let slotsAndRooms :: [((DayIndex, SlotIndex), RoomID)]
      slotsAndRooms =
        concatMap
            ( (\(s, roomIDs) -> [ (s, roomID') | roomID' <- roomIDs ])
            . second (map roomID . concatMap rooms . M.elems . examsInSlot)
            )
          $ M.toList
          $ slots plan
      roomSlots' :: M.Map RoomID [(DayIndex, SlotIndex)]
      roomSlots' = roomSlots $ constraints plan
  tell [Info "### Validating if all rooms in a slot are allowed (hard)"]
  allRoomsInAllowedSlots <- forM slotsAndRooms
    $ validateRoomAllowedInSlot roomSlots'
  return $ validationResult allRoomsInAllowedSlots

validateRoomAllowedInSlot
  :: M.Map RoomID [(DayIndex, SlotIndex)]
  -> ((DayIndex, SlotIndex), RoomID)
  -> Writer [ValidationRecord] ValidationResult
validateRoomAllowedInSlot roomSlots' (slot', roomID') = do
  let slotsForRoom = M.lookup roomID' roomSlots'
  case slotsForRoom of
    Nothing -> do
      tell [Info $ "No constraint for room " `append` showt roomID']
      return EverythingOk
    Just slots' -> if slot' `elem` slots'
      then return EverythingOk
      else do
        tell
          [ HardConstraintBroken
            $        showt roomID'
            `append` " not allowed in slot "
            `append` showt slot'
          ]
        return HardConstraintsBroken



