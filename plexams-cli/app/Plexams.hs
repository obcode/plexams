module Main where

import           Plexams.CLI.Commands
import           Plexams.CLI.Config
import           Plexams.CLI.PlanManip
import           Plexams.CLI.Types

main :: IO ()
main = configmain main'

main' :: Config -> IO ()
main' config = do

  unscheduledPlan <- makePlan config
  scheduledPlan <- applyPlanManips config unscheduledPlan
  scheduledPlanWithRooms <- applyAddRooms config scheduledPlan

  scheduledPlanWithRoomsAndInvigilators
                <- addInvigilatorsToPlan config scheduledPlanWithRooms

  runCommand (optCommand config) config scheduledPlanWithRoomsAndInvigilators
