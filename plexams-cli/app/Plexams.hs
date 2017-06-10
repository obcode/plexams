module Main where

import           Plexams.CLI.Commands
import           Plexams.CLI.Config
import           Plexams.CLI.PlanManip
import           Plexams.CLI.Types

main :: IO ()
main = configmain main'

main' :: Config -> IO ()
main' config =
  makePlan config >>=
  applyPlanManips config >>=
  applyAddRooms config >>=
  addInvigilatorsToPlan config >>=
  applyAddInvigilators config >>=
  runCommand (optCommand config) config
