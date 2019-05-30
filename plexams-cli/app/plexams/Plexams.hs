module Main where

import           Data.Text                      ( unpack )
import           System.Directory               ( getCurrentDirectory
                                                , setCurrentDirectory
                                                )

import           Plexams.CLI.Commands
import           Plexams.CLI.Config
import           Plexams.CLI.Types
import           Plexams.Import                 ( importPlan
                                                , setPlexamsDirectory
                                                )

main :: IO ()
main = configmain main'

main' :: Config -> IO ()
main' config = do
  currentDir <- getCurrentDirectory
  setPlexamsDirectory
  (maybePlan, messages) <- importPlan
  mapM_ (putStrLn . unpack) messages
  case maybePlan of
    Nothing    -> putStrLn "error: no plan"
    Just plan' -> runCommand (optCommand config) config plan'
  setCurrentDirectory currentDir
