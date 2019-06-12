{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TupleSections #-}

module Plexams.Validation.Rooms
  ( validate
  )
where

import           Control.Arrow                  ( second )
import           Control.Monad.Writer
-- import           Data.List                      ( nub )
import qualified Data.Map                      as M
import           Data.Text                      ( append
                                                , pack
                                                )

import           TextShow                       ( showt )

import           Plexams.Types

validate :: Plan -> Writer [ValidationRecord] ValidationResult
validate plan = do
  tell [ValidationRecord Info "## Validating Rooms"]
  -- normalRoomsNoHandicap
  -- handicapRoomsAllHandicap
  -- no student left outside of room
  enoughRoomsForExams       <- validateEnoughRoomsForExams plan
  stillReserveForExams      <- validationStillReserveForExams plan
  -- differentRoomsInSlot     <- validateDifferentRoomsInSlots plan
  noStudentLeftOutsideRoom  <- validateNoStudentLeftOutsideRoom plan
  allRoomsAllowedInSlots    <- validateRoomsAllowedInSlots plan
  handicapRoomAlone         <- validateHandicapRoomAlone plan
  handicapRoomNotInNextSlot <- validateHandicapRoomsNotInNextSlot plan
  -- TODO: roomSlots eingehalten, inSameRoom eingehalten?
  return $ validationResult
    [ enoughRoomsForExams
    , stillReserveForExams
    -- , differentRoomsInSlot
    , noStudentLeftOutsideRoom
    , allRoomsAllowedInSlots
    , handicapRoomAlone
    , handicapRoomNotInNextSlot
    ]

validateNoStudentLeftOutsideRoom
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateNoStudentLeftOutsideRoom plan = do
  tell [ValidationRecord Info "### Validation that all students have a seat"]
  validationResult <$> mapM validateNoStudentLeftOutsideRoomForExam
                            (filter plannedByMe $ scheduledExams plan)

validateNoStudentLeftOutsideRoomForExam
  :: Exam -> Writer [ValidationRecord] ValidationResult
validateNoStudentLeftOutsideRoomForExam exam =
  if null $ registeredStudents exam
    then return EverythingOk
    else do
      tell
        [ ValidationRecord HardConstraintBroken
          $        "- exam "
          `append` showt (anCode exam)
          `append` ": no room for "
          `append` showt (map studentName $ registeredStudents exam)
        ]
      return HardConstraintsBroken

validateEnoughRoomsForExams
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateEnoughRoomsForExams plan = do
  tell [ValidationRecord Info "### Validating enough rooms for exam (hard)"]
  validationResult <$> mapM validateEnoughRoomsForExam
                            (filter plannedByMe $ scheduledExams plan)

validateEnoughRoomsForExam :: Exam -> Writer [ValidationRecord] ValidationResult
validateEnoughRoomsForExam exam = do
  let regs' = registrations exam
      seats = sum $ map maxSeats $ rooms exam
  if regs' > seats
    then do
      tell
        [ ValidationRecord HardConstraintBroken
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
    [ ValidationRecord
        Info
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
        [ ValidationRecord SoftConstraintBroken
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
-- validateDifferentRoomsInSlots
--   :: Plan -> Writer [ValidationRecord] ValidationResult
-- validateDifferentRoomsInSlots plan = do
--   tell [ValidationRecord Info "### Validating different rooms in slot (hard)"]
--   validationResult <$> mapM validateDifferentRoomsInSlot (M.toList (slots plan))

-- validateDifferentRoomsInSlot
--   :: ((DayIndex, SlotIndex), Slot) -> Writer [ValidationRecord] ValidationResult
-- validateDifferentRoomsInSlot (index, slot') = do
--   let
--     exams' = examsInSlot slot'
--     roomsDifferent rs = length rs == length (nub (map (roomID . fst) rs))
--     plannedRooms      = concatMap (\e -> map (, e) $ rooms e) exams'
--     allRoomsDifferent = roomsDifferent plannedRooms
--     plannedRoomsWithoutReserveRooms =
--       filter (not . reserveRoom . fst) plannedRooms
--     plannedRoomsWithoutReserveRoomsDifferent =
--       roomsDifferent plannedRoomsWithoutReserveRooms
--     plannedRoomsWithoutHandicapCompensation =
--       filter (not . handicapCompensation . fst) plannedRooms
--     plannedRoomsWithoutHandicapCompensationDifferent =
--       roomsDifferent plannedRoomsWithoutHandicapCompensation
--   unless allRoomsDifferent $ tell
--     [ ValidationRecord HardConstraintBroken
--       $        "- slot "
--       `append` showt index
--       `append` ": same rooms used more than once"
--     ]
--   return $ if allRoomsDifferent
--     then EverythingOk
--     else
--       if plannedRoomsWithoutReserveRoomsDifferent
--            && plannedRoomsWithoutHandicapCompensationDifferent
--         then SoftConstraintsBroken
--         else HardConstraintsBroken

slotsAndRooms :: Plan -> [((DayIndex, SlotIndex), RoomID)]
slotsAndRooms =
  concatMap
      ( (\(s, roomIDs) -> [ (s, roomID') | roomID' <- roomIDs ])
      . second (map roomID . concatMap rooms . M.elems . examsInSlot)
      )
    . M.toList
    . slots

validateRoomsAllowedInSlots
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateRoomsAllowedInSlots plan = do
  let roomSlots' :: M.Map RoomID [(DayIndex, SlotIndex)]
      roomSlots' = roomSlots $ constraints plan
  tell
    [ ValidationRecord
        Info
        "### Validating if all rooms in a slot are allowed (hard)"
    ]
  allRoomsInAllowedSlots <- forM (slotsAndRooms plan)
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
      tell
        [ ValidationRecord Info
          $        "No constraint for room "
          `append` showt roomID'
        ]
      return EverythingOk
    Just slots' -> if slot' `elem` slots'
      then return EverythingOk
      else do
        tell
          [ ValidationRecord HardConstraintBroken
            $        showt roomID'
            `append` " not allowed in slot "
            `append` showt slot'
          ]
        return HardConstraintsBroken


validateHandicapRoomAlone :: Plan -> Writer [ValidationRecord] ValidationResult
validateHandicapRoomAlone plan = do
  let examsWithNTARoomAlone' = examsWithNTARoomAlone plan
  tell
    [ ValidationRecord Info
                       "### Validating if all handicaps in room alone (hard)"
    ]
  allHandicapsInRoomAloneOk <- forM examsWithNTARoomAlone'
    $ validateHandicapRoomAloneForExam plan
  return $ validationResult allHandicapsInRoomAloneOk


validateHandicapRoomAloneForExam
  :: Plan -> Exam -> Writer [ValidationRecord] ValidationResult
validateHandicapRoomAloneForExam plan exam = case slot exam of
  Nothing -> do
    tell
      [ ValidationRecord HardConstraintBroken
                         (pack $ name exam ++ " not planned")
      ]
    return HardConstraintsBroken
  Just slot' -> do
    let
      allRoomsInSlot =
        maybe [] (concatMap rooms . M.elems . examsInSlot)
          $ M.lookup slot'
          $ slots plan
      roomsWithHandicapRoomAlone = filter
        ( any (maybe False handicapNeedsRoomAlone . studentHandicap)
        . studentsInRoom
        )
        allRoomsInSlot
    allHandicapsInSlotRoomAloneOk <-
      forM roomsWithHandicapRoomAlone
        $ validateAllHandicapsInSlotRoomAloneOk exam allRoomsInSlot
    return $ validationResult allHandicapsInSlotRoomAloneOk

validateAllHandicapsInSlotRoomAloneOk
  :: Exam -> [Room] -> Room -> Writer [ValidationRecord] ValidationResult
validateAllHandicapsInSlotRoomAloneOk exam allRoomsInSlot roomWithHandicap = do
  let sameRooms = filter ((== roomID roomWithHandicap) . roomID) allRoomsInSlot
  if length sameRooms /= 1 || length (studentsInRoom roomWithHandicap) /= 1
    then do
      tell
        [ ValidationRecord
            HardConstraintBroken
            (  pack
            $  "Problems with handicap room alone for "
            ++ roomID roomWithHandicap
            ++ ", "
            ++ show (anCode exam)
            ++ ". "
            ++ show (name exam)
            ++ " in Slot "
            ++ show (slot exam)
            )
        ]
      return HardConstraintsBroken
    else return EverythingOk

validateHandicapRoomsNotInNextSlot
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateHandicapRoomsNotInNextSlot plan = do
  tell
    [ ValidationRecord
        Info
        "### Validating if all handicaps rooms are not used in the following slot (hard)"
    ]
  allHandicapsRoomsNotInNextSlot <- forM (examsWithNTA plan)
    $ validateHandicapRoomNotInNextSlot plan
  return $ validationResult allHandicapsRoomsNotInNextSlot

validateHandicapRoomNotInNextSlot
  :: Plan -> Exam -> Writer [ValidationRecord] ValidationResult
validateHandicapRoomNotInNextSlot plan exam = case slot exam of
  Nothing -> do
    tell
      [ ValidationRecord HardConstraintBroken
        $  pack
        $  name exam
        ++ " not planned"
      ]
    return HardConstraintsBroken
  Just (d, s) -> do
    let roomsWithHandicap = filter handicapCompensation $ rooms exam
    roomsOk <- forM roomsWithHandicap
      $ validateHandicapRoomNotInSlot plan (d, s + 1)
    return $ validationResult roomsOk

validateHandicapRoomNotInSlot
  :: Plan
  -> (DayIndex, SlotIndex)
  -> Room
  -> Writer [ValidationRecord] ValidationResult
validateHandicapRoomNotInSlot plan slotIndex room =
  case M.lookup slotIndex $ slots plan of
    Nothing    -> return EverythingOk
    Just slot' -> do
      let roomsInSlot = concatMap rooms $ M.elems $ examsInSlot slot'
      if roomID room `elem` map roomID roomsInSlot
        then do
          tell
            [ ValidationRecord HardConstraintBroken
              $  pack
              $  "Handicap room "
              ++ roomID room
              ++ " used again in slot "
              ++ show slotIndex
            ]
          return HardConstraintsBroken
        else return EverythingOk


