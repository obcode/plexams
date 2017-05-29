module Plexams.Validation
    ( validate
    , module Plexams.Validation.Exports
    ) where

import           Control.Arrow                     ((&&&))
import           Control.Monad.Writer
import           Data.List                         (nub)
import qualified Data.Map                          as M
import           Data.Maybe                        (isJust, mapMaybe)
import           GHC.Exts                          (groupWith)
import           Plexams.Query
import           Plexams.Types
import           Plexams.Validation.Exports
import qualified Plexams.Validation.Rooms
import qualified Plexams.Validation.ScheduledExams
import qualified Plexams.Validation.Sources

-- validateZPAExport = Plexams.Validation.Exports.validateZPAExport

validate :: Plan -> (ValidationResult, [String])
validate = runWriter . validate'

validate' :: Plan -> Writer [String] ValidationResult
validate' plan = do
  tell ["# Validation"]
  sourcesOk <- Plexams.Validation.Sources.validate plan
  lecturersMax3ExamDays <- validateLecturersMax3ExamDays plan
  scheduledExams <- Plexams.Validation.ScheduledExams.validate plan
  roomsOk <- Plexams.Validation.Rooms.validate plan
  tell ["# no more validations implemented yet"]
  return $ validationResult
            [ sourcesOk
            , lecturersMax3ExamDays
            , scheduledExams
            ]

validateLecturersMax3ExamDays :: Plan -> Writer [String] ValidationResult
validateLecturersMax3ExamDays plan = do
  let lecturerWithMoreThan3ExamDays =
          filter ((>3) . length . nub . snd) $ lecturerExamDays plan
      ok = null lecturerWithMoreThan3ExamDays
  tell ["# Checking amount of exam days for each lecturer (soft)"]
  unless ok $
    mapM_ (\(l,d) ->
              tell ["- More than 3 days of exams: "
                    ++ personShortName l
                    ++ ": " ++ show d])
          lecturerWithMoreThan3ExamDays
  return $ if ok then EverythingOk else SoftConstraintsBroken
