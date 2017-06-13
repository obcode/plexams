{-# LANGUAGE OverloadedStrings #-}
module Plexams.Validation.Invigilation
  ( validate
  ) where

import           Control.Arrow        (second, (&&&))
import           Control.Monad.Writer
import           Data.List            (intersect, nub, (\\))
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, isNothing, mapMaybe)
import           Data.Text            (Text, append, pack)
import           GHC.Exts             (groupWith, sortWith)
import           Plexams.Invigilation
import           Plexams.Types
import           TextShow             (showt)

validate :: Plan -> Writer [Text] ValidationResult
validate plan = do
  tell ["## Validating Invigilations"]
  allSlotsHaveReserves <- validateAllSlotsHaveReserves plan
  allRoomsHaveInvigilators <- validateAllRoomsHaveInvigilators plan
  invigilatorOk <- validateInvigilator plan
  handicapsOk <- validateHandicapsInvigilator plan
  return $ validationResult
          [ allSlotsHaveReserves
          , allRoomsHaveInvigilators
          , invigilatorOk
          , handicapsOk
          ]

validateAllSlotsHaveReserves :: Plan -> Writer [Text] ValidationResult
validateAllSlotsHaveReserves plan = do
  tell ["### Validating all slots have a reserve invigilator"]
  let slotsWithoutReserve = filter (isNothing . snd)
                          $ map (second reserveInvigilator)
                          $ filter (not . null . M.elems . examsInSlot . snd)
                          $ M.toList
                          $ slots plan
  forM_ slotsWithoutReserve $ \(s, _) ->
    tell ["- Slot " `append` showt s `append` ": no reserve defined"]
  return $ if null slotsWithoutReserve
           then EverythingOk
           else HardConstraintsBroken

validateAllRoomsHaveInvigilators :: Plan -> Writer [Text] ValidationResult
validateAllRoomsHaveInvigilators plan = do
  tell ["### Validating all rooms have an invigilator"]
  let allRoomsWithoutInvigilator = filter (isNothing . snd)
                                 $ concatMap (\exam' ->
                                      map (\r -> ( (anCode exam', roomID r)
                                                 , invigilator r))
                                          (rooms exam')
                                    )
                                 $ scheduledExams plan
  forM_ allRoomsWithoutInvigilator $ \((ancode,roomID'), _) ->
    tell ["- Exam " `append` showt ancode `append`
          ", Room " `append` pack roomID' `append` ": no invigilator defined"]
  return $ if null allRoomsWithoutInvigilator
           then EverythingOk
           else HardConstraintsBroken

validateInvigilator  :: Plan -> Writer [Text] ValidationResult
validateInvigilator plan = do
  tell ["### Validating invigilators constraints"]
  let reserveInvigilatorIds = map invigilatorID
                            $ mapMaybe reserveInvigilator
                            $ M.elems $ slots plan
      invigilatorInRoomIds =  map invigilatorID
                           $ mapMaybe invigilator
                           $ concatMap (concatMap rooms . M.elems . examsInSlot)
                           $ M.elems $ slots plan
      invigilatorIds = M.keys $ invigilators plan
  validationResult <$> mapM (validateInvigilator' plan)
                            (nub (   reserveInvigilatorIds
                                  ++ invigilatorInRoomIds
                                  ++ invigilatorIds))

validateInvigilator' :: Plan -> PersonID -> Writer [Text] ValidationResult
validateInvigilator' plan' invigilatorID' = do
  let plan = setSlotsOnExams plan'
      maybeInvigilator = M.lookup invigilatorID' $ invigilators plan
      invigilationsPerPerson' = invigilationsPerPerson plan
  case maybeInvigilator of
    Nothing -> do
      tell ["- InvigilatorID " `append` showt invigilatorID'
                     `append` " not found in invigilators"]
      return SoftConstraintsBroken
    Just invigilator' -> do
      let invigilations' = M.findWithDefault [] (invigilatorID invigilator')
                                                invigilationsPerPerson'
          invigilationDays = nub $ map invigilationDay invigilations'
          maxThreeDays = if length invigilationDays <= 3
                         then EverythingOk
                         else SoftConstraintsBroken
          daysOk
            | null (invigilationDays \\ invigilatorWantDays invigilator')
                = EverythingOk
            | null ((invigilationDays \\ invigilatorWantDays invigilator')
                                      \\ invigilatorCanDays invigilator')
                = SoftConstraintsBroken
            | otherwise = HardConstraintsBroken
          minutesLeft = invigilatorMinutesTodo invigilator'
                      - invigilatorsMinutesPlanned invigilator'
          numberOfMinutesOk
            | minutesLeft > -90 = EverythingOk
            | otherwise = SoftConstraintsBroken
          notMoreThanOnceInSlot =
            filter ((>1) . length)
            $ map (nub . map invigilationRoom)
            $ groupWith (invigilationDay &&& invigilationSlot)
                        invigilations'
          notMoreThanOnceInSlotOk
            | null notMoreThanOnceInSlot = EverythingOk
            | otherwise = HardConstraintsBroken
          -- TODO: Prüfen wenn Aufsicht und Prüfer, dann nur dieser Raum
          ownReserveSlots =
            map fst
            $ filter ((==Just True) . snd)
            $ map (second (fmap ((==invigilatorID') . invigilatorID)
                            . reserveInvigilator
                          )
                  )
            $ M.toList
            $ slots plan
          ownExamSlots =
            mapMaybe slot
            $ filter ((==invigilatorID') . personID . lecturer)
            $ scheduledExams plan
          ownInvigilatorSlots =
            mapMaybe slot
            $ filter (\e -> not (   personID (lecturer e) == invigilatorID'
                                 && length (rooms e) == 1
                                )
                     )
            $ filter (elem (Just invigilatorID')
                . map (fmap invigilatorID . invigilator)
                . rooms)
            $ scheduledExams plan
          notReserveOrInvigilatorIfExamInSlotOk =
            if null (ownExamSlots `intersect` ownReserveSlots)
              && null (ownExamSlots `intersect` ownInvigilatorSlots)
              && null (ownReserveSlots `intersect` ownInvigilatorSlots)
            then EverythingOk
            else HardConstraintsBroken
      unless (maxThreeDays == EverythingOk) $
        tell ["- " `append` invigilatorName invigilator'
                   `append` " invigilations on more than three days: "
                   `append` showt invigilationDays]
      when (daysOk == HardConstraintsBroken) $
        tell ["- " `append` invigilatorName invigilator'
                   `append` " invigilations on wrong days: "
                   `append` showt invigilationDays]
      when (daysOk == SoftConstraintsBroken) $
        tell ["- " `append` invigilatorName invigilator'
                   `append` " invigilations on can days: "
                   `append` showt invigilationDays]
      unless (numberOfMinutesOk == EverythingOk) $
        tell ["- " `append` invigilatorName invigilator'
                   `append` " has too much invigilations "
                   `append` showt minutesLeft]
      unless (notMoreThanOnceInSlotOk == EverythingOk) $
        tell ["- " `append` invigilatorName invigilator'
                   `append` " has been scheduled more then once in slot  "
                   `append` showt notMoreThanOnceInSlot]
      unless (notReserveOrInvigilatorIfExamInSlotOk == EverythingOk) $
        tell ["- " `append` invigilatorName invigilator'
                   `append` " has been scheduled as a reserve or invigilator"
                   `append` " during his or her own exam (exam, invig, reserve)"
                   `append` showt ( ownExamSlots
                                  , ownInvigilatorSlots
                                  , ownReserveSlots)]
      return $ validationResult [ maxThreeDays
                                , daysOk
                                , numberOfMinutesOk
                                , notMoreThanOnceInSlotOk
                                , notReserveOrInvigilatorIfExamInSlotOk
                                ]

validateHandicapsInvigilator :: Plan -> Writer [Text] ValidationResult
validateHandicapsInvigilator plan = do
  -- a invigilator in a handicaps room should not be involved in the
  -- following slot
  tell ["### Validating handicap invigilators constraints"]
  let days' = map slotAndNextSlot $ groupWith (fst . fst) $ M.toList $ slots plan
      slotAndNextSlot day' =
        let slots' = sortWith (snd . fst) day'
        in zip slots' (tail slots')
  validationResult <$>
    mapM (\d -> validationResult <$> mapM validateHandicapsInvigilator' d)
         days'

validateHandicapsInvigilator' :: ( ((DayIndex, SlotIndex), Slot)
                                 , ((DayIndex, SlotIndex), Slot))
                              -> Writer [Text] ValidationResult
validateHandicapsInvigilator' ((si@(d1,s1), slot'), ((d2,s2), nextSlot)) = do
  when (d1 /= d2) $
    tell [">>> This should not happen. Validating handicaps on different days"]
  when (s1 + 1 /= s2) $
    tell
     [">>> This should not happen. Validating handicaps on non-following slots"]
  let handicapInvigilators =
        nub
        $ catMaybes
        $ concatMap ( map (fmap invigilatorID . invigilator)
                  . filter handicapCompensation
                  . rooms)
        $ M.elems
        $ examsInSlot slot'
      personsInvolvedInNextSlot =
        nub
        $ catMaybes (
          (invigilatorID <$> reserveInvigilator nextSlot)
          : concatMap (\e ->
                Just (personID $ lecturer e)
                : map (fmap invigilatorID . invigilator) (rooms e)
              )
              ( M.elems
                $ examsInSlot nextSlot
              )
        )
      handicapInvigilatorInvolvedInNextSlot =
        any (`elem` personsInvolvedInNextSlot) handicapInvigilators
  when handicapInvigilatorInvolvedInNextSlot $
    tell ["- Handicap invigilator (slot " `append` showt si
          `append` ") is involved in next slot"]
  return $ if handicapInvigilatorInvolvedInNextSlot
           then HardConstraintsBroken
           else EverythingOk
