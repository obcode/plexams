{-# LANGUAGE OverloadedStrings #-}
module Plexams.Validation.Invigilation
  ( validate
  ) where

import           Control.Arrow        (second)
import           Control.Monad.Writer
import           Data.List            (nub)
import qualified Data.Map             as M
import           Data.Maybe           (isNothing, mapMaybe)
import           Data.Text            (Text, append, pack)
import           Plexams.Types
import           TextShow             (showt)

validate :: Plan -> Writer [Text] ValidationResult
validate plan = do
  tell ["## Validating Invigilations"]
  allSlotsHaveReserves <- validateAllSlotsHaveReserves plan
  allRoomsHaveInvigilators <- validateAllRoomsHaveInvigilators plan
  invigilatorOk <- validateInvigilator plan
  return $ validationResult
          [ allSlotsHaveReserves
          , allRoomsHaveInvigilators
          , invigilatorOk
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

validateInvigilator'  :: Plan -> PersonID -> Writer [Text] ValidationResult
validateInvigilator' plan invigilatorID' = do
  let maybeInvigilator = M.lookup invigilatorID' $ invigilators plan
  case maybeInvigilator of
    Nothing ->
      tell ["- InvigilatorID " `append` showt invigilatorID'
                     `append` " not found in invigilators"]
    Just invigilator' ->
      tell ["- Checking Invigilator " `append` invigilatorName invigilator']
  -- TODO:
  -- 1. Offene Zeiten +-90
  -- 2. nie gleichzeitig in mehreren Räumen
  -- 3. Tage eingehalten?
  -- 4. max 3 Tage
  return HardConstraintsBroken
