module Main where

import Plexams.CLI.Helper.Commands
import Plexams.CLI.Helper.Config
import Plexams.CLI.Helper.Types

main :: IO ()
main = configmain main'

main' :: Config -> IO ()
main' = runCommand
