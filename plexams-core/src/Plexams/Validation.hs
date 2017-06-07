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
import qualified Plexams.Validation.Rooms
import qualified Plexams.Validation.ScheduledExams
import qualified Plexams.Validation.Sources
import           TextShow                          (showt)

validate :: Plan -> (ValidationResult, [Text])
validate = runWriter . validate'

validate' :: Plan -> Writer [Text] ValidationResult
validate' plan = do
  tell ["# Validation"]
  sourcesOk <- Plexams.Validation.Sources.validate plan
  lecturersMax3ExamDays <- validateLecturersMax3ExamDays plan
  scheduledExams' <- Plexams.Validation.ScheduledExams.validate plan
  roomsOk <- Plexams.Validation.Rooms.validate plan
  tell ["# no more validations implemented yet"]
  return $ validationResult
            [ sourcesOk
            , lecturersMax3ExamDays
            , scheduledExams'
            , roomsOk
            ]

validateLecturersMax3ExamDays :: Plan -> Writer [Text] ValidationResult
validateLecturersMax3ExamDays plan = do
  let lecturerWithMoreThan3ExamDays =
          filter ((>3) . length . nub . snd) $ lecturerExamDays plan
      ok = null lecturerWithMoreThan3ExamDays
  tell ["# Checking amount of exam days for each lecturer (soft)"]
  unless ok $
    mapM_ (\(l,d) ->
              tell ["- More than 3 days of exams: "
                    `append` showt (personShortName l)
                    `append` ": "
                    `append` showt d])
          lecturerWithMoreThan3ExamDays
  return $ if ok then EverythingOk else SoftConstraintsBroken
