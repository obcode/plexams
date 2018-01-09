module Plexams.Export.Common
  ( unscheduledExamsSortedByRegistrations
  , notPlannedByMeExams
  ) where

import Data.List (sortBy)
import qualified Data.Map as M

import Plexams.Types

unscheduledExamsSortedByRegistrations :: Plan -> [Exam]
unscheduledExamsSortedByRegistrations =
  sortBy (\e1 e2 -> compare (registrations e2) (registrations e1)) .
  filter plannedByMe . M.elems . unscheduledExams

notPlannedByMeExams :: Plan -> [Exam]
notPlannedByMeExams = filter (not . plannedByMe) . M.elems . unscheduledExams
