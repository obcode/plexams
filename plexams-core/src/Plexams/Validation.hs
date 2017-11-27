{-# LANGUAGE OverloadedStrings #-}
module Plexams.Validation
    ( validate
    , module Plexams.Validation.Exports
    ) where

import           Control.Monad.Writer
import           Data.List                         (nub)
import           Data.Text                         (Text, append)
import           Plexams.Query
import           Plexams.Types
import           Plexams.Validation.Exports
import qualified Plexams.Validation.Invigilation
import qualified Plexams.Validation.Rooms
import qualified Plexams.Validation.ScheduledExams
import qualified Plexams.Validation.Sources
import           TextShow                          (showt)

validate :: [ValidateWhat] -> Plan -> (ValidationResult, [ValidationRecord])
validate validateWhat = runWriter . validate' validateWhat

validate' :: [ValidateWhat] -> Plan -> Writer [ValidationRecord] ValidationResult
validate' validateWhat plan = do
  tell [Info "# Validation"]
  sourcesOk <- if ValidateSources `elem` validateWhat
    then Plexams.Validation.Sources.validate plan
    else do
      tell [Info "no validation of sources requested"]
      return EverythingOk
  scheduledExams' <- if ValidateSchedule `elem` validateWhat
    then do
      scheduledExams'' <- Plexams.Validation.ScheduledExams.validate plan
      max3days <- validateLecturersMax3ExamDays plan
      return $ validationResult [ scheduledExams'', max3days]
    else do
      tell [Info "no validation of schedule requested"]
      return EverythingOk
  roomsOk <- if ValidateRooms `elem` validateWhat
    then Plexams.Validation.Rooms.validate plan
    else do
      tell [Info "no validation of rooms requested"]
      return EverythingOk
  invigilationsOk <- if ValidateInvigilation `elem` validateWhat
    then Plexams.Validation.Invigilation.validate plan
    else do
      tell [Info "no validation of invigilation requested"]
      return EverythingOk
  tell [Info "# no more validations implemented yet"]
  return $ validationResult
            [ sourcesOk
            , scheduledExams'
            , roomsOk
            , invigilationsOk
            ]

validateLecturersMax3ExamDays :: Plan -> Writer [ValidationRecord] ValidationResult
validateLecturersMax3ExamDays plan = do
  let lecturerWithMoreThan3ExamDays =
          filter ((>3) . length . nub . snd) $ lecturerExamDays plan
      ok = null lecturerWithMoreThan3ExamDays
  tell [Info "# Checking amount of exam days for each lecturer (soft)"]
  unless ok $
    mapM_ (\(l,d) ->
              tell [Info $ "- More than 3 days of exams: "
                    `append` showt (personShortName l)
                    `append` ": "
                    `append` showt d])
          lecturerWithMoreThan3ExamDays
  return $ if ok then EverythingOk else SoftConstraintsBroken
