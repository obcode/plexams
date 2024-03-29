{-# LANGUAGE OverloadedStrings #-}

module Plexams.Validation
  ( validate
  , module Plexams.Validation.Exports
  )
where

import           Control.Monad.Writer
-- import           Data.List                      ( nub )
-- import           Data.Text                      ( append )
-- import           TextShow                       ( showt )

-- import           Plexams.Query
import           Plexams.Types
import           Plexams.Validation.Exports
import qualified Plexams.Validation.Invigilation
import qualified Plexams.Validation.Rooms
import qualified Plexams.Validation.ScheduledExams
import qualified Plexams.Validation.Sources

validate :: [ValidateWhat] -> Plan -> (ValidationResult, [ValidationRecord])
validate validateWhat' = runWriter . validate' validateWhat'

validate'
  :: [ValidateWhat] -> Plan -> Writer [ValidationRecord] ValidationResult
validate' validateWhat' plan = do
  tell [ValidationRecord Info "# Validation"]
  sourcesOk       <- Plexams.Validation.Sources.validate plan
  scheduledExams' <- if ValidateSchedule `elem` validateWhat'
    then do
      scheduledExams'' <- Plexams.Validation.ScheduledExams.validate plan
      -- max3days         <- validateLecturersMax3ExamDays plan
      return $ validationResult [scheduledExams'']
    else return EverythingOk
  roomsOk <- if ValidateRooms `elem` validateWhat'
    then Plexams.Validation.Rooms.validate plan
-- tell [Info "no validation of rooms requested"]
    else return EverythingOk
  invigilationsOk <- if ValidateInvigilation `elem` validateWhat'
    then Plexams.Validation.Invigilation.validate plan
-- tell [Info "no validation of invigilation requested"]
    else return EverythingOk
  tell [ValidationRecord Info "# no more validations implemented yet"]
  return
    $ validationResult [sourcesOk, scheduledExams', roomsOk, invigilationsOk]

-- validateLecturersMax3ExamDays ::
--      Plan -> Writer [ValidationRecord] ValidationResult
-- validateLecturersMax3ExamDays plan = do
--   let lecturerWithMoreThan3ExamDays =
--         filter ((> 3) . length . nub . snd) $ lecturerExamDays plan
--       ok = null lecturerWithMoreThan3ExamDays
--   tell [ValidationRecord Info "# Checking amount of exam days for each lecturer (hard)"]
--   unless ok $
--     mapM_
--       (\(l, d) ->
--          tell
--            [ ValidationRecord HardConstraintBroken $
--              "- More than 3 days of exams: " `append` showt (personShortName l) `append`
--              ": " `append`
--              showt d
--            ])
--       lecturerWithMoreThan3ExamDays
--   return $
--     if ok
--       then EverythingOk
--       else SoftConstraintsBroken
