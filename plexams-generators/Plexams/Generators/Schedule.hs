module Plexams.Generators.Schedule
  ( generateSchedule
  ) where

import           Plexams.Types

generateSchedule :: Plan -> (Plan, [PlanManip])
generateSchedule plan = undefined

findSlotForUnscheduledExam :: Integer -> Plan -> PlanManip
findSlotForUnscheduledExam ancode plan = undefined
