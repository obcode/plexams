module Main where

import           Plexams.GUI
import           Plexams.Types
import           Semester

emptyPlan = makeEmptyPlan semesterConfig

main :: IO ()
main =
  do
      print emptyPlan
      mainGUI
