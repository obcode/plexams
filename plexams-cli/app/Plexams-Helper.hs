module Main where

import           Plexams.CLI.Helper.Commands
import           Plexams.CLI.Helper.Types
import Plexams.CLI.Helper.Config

main :: IO ()
main = configmain main'

main' :: Config -> IO ()
main' = runCommand
