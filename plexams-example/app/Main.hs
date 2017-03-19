module Main where

import           Plexams
import           Plexams.Export
import           Plexams.GUI
import           Plexams.Types

main :: IO ()
main =
  do
      maybeSemesterConfig <- initSemesterConfigFromFile "./plexams-config.json"
      case maybeSemesterConfig of
          Nothing -> putStrLn "no semester config"
          Just semesterConfig -> putStrLn $ planToMD $ makeEmptyPlan semesterConfig
      mainGUI
