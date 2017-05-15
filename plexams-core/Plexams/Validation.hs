module Plexams.Validation
    ( validate
    ) where

import           Control.Arrow                     ((&&&))
import           Control.Monad.Writer
import           Data.List                         (nub)
import qualified Data.Map                          as M
import           Data.Maybe                        (isJust, mapMaybe)
import           GHC.Exts                          (groupWith)
import           Plexams.Query
import           Plexams.Types
import qualified Plexams.Validation.ScheduledExams
import qualified Plexams.Validation.Sources

validate :: Plan -> (Bool, [String])
validate = runWriter . validate'

validate' :: Plan -> Writer [String] Bool
validate' plan = do
  tell ["# Validation"]
  sourcesOk <- Plexams.Validation.Sources.validate plan
  lecturersMax3ExamDays <- validateLecturersMax3ExamDays plan
  scheduledExams <- Plexams.Validation.ScheduledExams.validate plan
  -- TODO: overlaps in adjacent slots
  -- TODO: overlaps on sameday
  tell ["# no more validations implemented yet"]
  return $ lecturersMax3ExamDays
        && scheduledExams
        && False


validateLecturersMax3ExamDays :: Plan -> Writer [String] Bool
validateLecturersMax3ExamDays plan = do
  let lecturerWithMoreThan3ExamDays =
          filter ((>3) . length . snd) $ lecturerExamDays plan
      ok = null lecturerWithMoreThan3ExamDays
  tell ["# Checking amount of exam days for each lecturer"]
  unless ok $
    mapM_ (\(l,d) ->
              tell ["- More than 3 days of exams: "
                    ++ personShortName l
                    ++ ": " ++ show d])
          lecturerWithMoreThan3ExamDays
  return ok
