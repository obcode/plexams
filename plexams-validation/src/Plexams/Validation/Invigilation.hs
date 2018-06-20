{-# LANGUAGE OverloadedStrings #-}

module Plexams.Validation.Invigilation
  ( validate
  ) where

import Control.Arrow ((&&&), second)
import Control.Monad.Writer
import Data.List ((\\), intersect, nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Text (append, pack)
import GHC.Exts (groupWith, sortWith)

import TextShow (showt)

import Plexams.Invigilation
import Plexams.Types

validate :: Plan -> Writer [ValidationRecord] ValidationResult
validate plan = do
  tell [Info "## Validating Invigilations"]
  invigilatorOk <- validateInvigilator plan
  handicapsOk <- validateHandicapsInvigilator plan
  allSlotsHaveReserves <- validateAllSlotsHaveReserves plan
  allRoomsHaveInvigilators <- validateAllRoomsHaveInvigilators plan
  return $
    validationResult
      [ allSlotsHaveReserves
      , allRoomsHaveInvigilators
      , invigilatorOk
      , handicapsOk
      ]

validateAllSlotsHaveReserves ::
     Plan -> Writer [ValidationRecord] ValidationResult
validateAllSlotsHaveReserves plan = do
  tell [Info "### Validating all slots have a reserve invigilator"]
  let slotsWithoutReserve =
        filter (isNothing . snd) $
        map (second reserveInvigilator) $
        filter (any plannedByMe . M.elems . examsInSlot . snd) $
        M.toList $ slots plan
  forM_ slotsWithoutReserve $ \(s, _) ->
    tell
      [ HardConstraintBroken $
        "- Slot " `append` showt s `append` ": no reserve defined"
      ]
  return $
    if null slotsWithoutReserve
      then EverythingOk
      else HardConstraintsBroken

validateAllRoomsHaveInvigilators ::
     Plan -> Writer [ValidationRecord] ValidationResult
validateAllRoomsHaveInvigilators plan = do
  tell [Info "### Validating all rooms have an invigilator"]
  let allRoomsWithoutInvigilator =
        filter (isNothing . snd) $
        concatMap
          (\exam' ->
             map (\r -> ((anCode exam', roomID r), invigilator r)) (rooms exam')) $
        scheduledExams plan
  forM_ allRoomsWithoutInvigilator $ \((ancode, roomID'), _) ->
    tell
      [ HardConstraintBroken $
        "- Exam " `append` showt ancode `append` ", Room " `append` pack roomID' `append`
        ": no invigilator defined"
      ]
  return $
    if null allRoomsWithoutInvigilator
      then EverythingOk
      else HardConstraintsBroken

validateInvigilator :: Plan -> Writer [ValidationRecord] ValidationResult
validateInvigilator plan' = do
  tell [Info "### Validating invigilators constraints"]
  let plan = setSlotsOnExams plan'
      slots' = M.elems $ slots plan
      reserveInvigilatorIds =
        map invigilatorID $ mapMaybe reserveInvigilator slots'
      invigilatorInRoomIds =
        map invigilatorID $
        mapMaybe invigilator $
        concatMap (concatMap rooms . M.elems . examsInSlot) slots'
      invigilatorIds = M.keys $ invigilators plan
      invigilationsPerPerson' = invigilationsPerPerson plan
      invigilationsOfPerson invigID =
        M.findWithDefault [] invigID invigilationsPerPerson'
  validationResult <$>
    mapM
      (\invigID ->
         validateInvigilator' plan invigID (invigilationsOfPerson invigID))
      (nub (reserveInvigilatorIds ++ invigilatorInRoomIds ++ invigilatorIds))

validateInvigilator' ::
     Plan
  -> PersonID
  -> [Invigilation]
  -> Writer [ValidationRecord] ValidationResult
validateInvigilator' _ _ [] = return EverythingOk -- ?
validateInvigilator' plan invigilatorID' invigilations' = do
  let maybeInvigilator = M.lookup invigilatorID' $ invigilators plan
  case maybeInvigilator of
    Nothing -> do
      tell
        [ SoftConstraintBroken $
          "- InvigilatorID " `append` showt invigilatorID' `append`
          " not found in invigilators"
        ]
      return SoftConstraintsBroken
    Just invigilator' -> do
      let invigilationDays = nub $ map invigilationDay invigilations'
          maxThreeDays =
            if length invigilationDays <= 3
              then EverythingOk
              else SoftConstraintsBroken
      unless (maxThreeDays == EverythingOk) $
        tell
          [ SoftConstraintBroken $
            "- " `append` invigilatorName invigilator' `append`
            " invigilations on more than three days: " `append`
            showt invigilationDays
          ]
      let daysOk
            | null (invigilationDays \\ invigilatorWantDays invigilator') =
              EverythingOk
            | null
               ((invigilationDays \\ invigilatorWantDays invigilator') \\
                invigilatorCanDays invigilator') = SoftConstraintsBroken
            | otherwise = HardConstraintsBroken
      when (daysOk == HardConstraintsBroken) $
        tell
          [ HardConstraintBroken $
            "- " `append` invigilatorName invigilator' `append`
            " invigilations on wrong days: " `append`
            showt invigilationDays
          ]
      when (daysOk == SoftConstraintsBroken) $
        tell
          [ SoftConstraintBroken $
            "- " `append` invigilatorName invigilator' `append`
            " invigilations on can days: " `append`
            showt invigilationDays
          ]
      let minutesLeft =
            invigilatorMinutesTodo invigilator' -
            invigilatorsMinutesPlanned invigilator'
          numberOfMinutesOk
            | minutesLeft > -90 = EverythingOk
            | otherwise = SoftConstraintsBroken
      unless (numberOfMinutesOk == EverythingOk) $
        tell
          [ SoftConstraintBroken $
            "- " `append` invigilatorName invigilator' `append`
            " has too much invigilations " `append`
            showt minutesLeft
          ]
      let notMoreThanOnceInSlot =
            filter ((> 1) . length) $
            map
              (nub .
               map
                 (\i ->
                    (invigilationDay i, invigilationSlot i, invigilationRoom i))) $
            groupWith (invigilationDay &&& invigilationSlot) invigilations'
          notMoreThanOnceInSlotOk
            | null notMoreThanOnceInSlot = EverythingOk
            | otherwise = HardConstraintsBroken
      unless (notMoreThanOnceInSlotOk == EverythingOk) $
        tell
          [ HardConstraintBroken $
            "- " `append` invigilatorName invigilator' `append`
            " has been scheduled more then once in slot  " `append`
            showt notMoreThanOnceInSlot
          ]
      let ownReserveSlots =
            map fst $
            filter ((== Just True) . snd) $
            map
              (second
                 (fmap ((== invigilatorID') . invigilatorID) .
                  reserveInvigilator)) $
            M.toList $ slots plan
          ownExamSlots =
            mapMaybe slot $
            filter ((== invigilatorID') . personID . lecturer) $
            scheduledExams plan
          ownInvigilatorSlots =
            mapMaybe slot $
            filter
              (\e ->
                 not
                   (personID (lecturer e) == invigilatorID' &&
                    length (rooms e) == 1)) $
            filter
              (elem (Just invigilatorID') .
               map (fmap invigilatorID . invigilator) . rooms) $
            scheduledExams plan
          notReserveOrInvigilatorIfExamInSlotOk =
            if null (ownExamSlots `intersect` ownReserveSlots) &&
               -- null (ownExamSlots `intersect` ownInvigilatorSlots) &&
               null (ownReserveSlots `intersect` ownInvigilatorSlots)
              then EverythingOk
              else HardConstraintsBroken
      unless (notReserveOrInvigilatorIfExamInSlotOk == EverythingOk) $
        tell
          [ HardConstraintBroken $
            "- " `append` invigilatorName invigilator' `append`
            " has been scheduled as a reserve and invigilator" `append`
            " during the same time, or reserve during own exams" `append`
            " (exam, invig, reserve)" `append`
            showt (ownExamSlots, ownInvigilatorSlots, ownReserveSlots)
          ]
      return $
        validationResult
          [ maxThreeDays
          , daysOk
          , numberOfMinutesOk
          , notMoreThanOnceInSlotOk
          , notReserveOrInvigilatorIfExamInSlotOk
          ]

validateHandicapsInvigilator ::
     Plan -> Writer [ValidationRecord] ValidationResult
validateHandicapsInvigilator plan
  -- a invigilator in a handicaps room should not be involved in the
  -- following slot
 = do
  tell [Info "### Validating handicap invigilators constraints"]
  let days' =
        map slotAndNextSlot $ groupWith (fst . fst) $ M.toList $ slots plan
      slotAndNextSlot day' =
        let slots' = sortWith (snd . fst) day'
        in zip slots' (tail slots')
  validationResult <$>
    mapM (fmap validationResult . mapM validateHandicapsInvigilator') days'

validateHandicapsInvigilator' ::
     (((DayIndex, SlotIndex), Slot), ((DayIndex, SlotIndex), Slot))
  -> Writer [ValidationRecord] ValidationResult
validateHandicapsInvigilator' ((si@(d1, s1), slot'), ((d2, s2), nextSlot)) = do
  when (d1 /= d2) $
    tell
      [ HardConstraintBroken
          ">>> This should not happen. Validating handicaps on different days"
      ]
  when (s1 + 1 /= s2) $
    tell
      [ HardConstraintBroken
          ">>> This should not happen. Validating handicaps on non-following slots"
      ]
  let handicapInvigilators =
        nub $
        catMaybes $
        concatMap
          (map (fmap invigilatorID . invigilator) .
           filter handicapCompensation . rooms) $
        M.elems $ examsInSlot slot'
      personsInvolvedInNextSlot =
        nub $
        catMaybes
          ((invigilatorID <$> reserveInvigilator nextSlot) :
           concatMap
             (\e ->
                Just (personID $ lecturer e) :
                map (fmap invigilatorID . invigilator) (rooms e))
             (M.elems $ examsInSlot nextSlot))
      handicapInvigilatorInvolvedInNextSlot =
        any (`elem` personsInvolvedInNextSlot) handicapInvigilators
  when handicapInvigilatorInvolvedInNextSlot $
    tell
      [ HardConstraintBroken $
        "- Handicap invigilator (slot " `append` showt si `append`
        ") is involved in next slot"
      ]
  return $
    if handicapInvigilatorInvolvedInNextSlot
      then HardConstraintsBroken
      else EverythingOk
