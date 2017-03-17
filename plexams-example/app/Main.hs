module Main where

import           Plexams.GUI
import           Plexams.Types

main :: IO ()
main =
  do
      print defaultBookableRooms
      mainGUI
