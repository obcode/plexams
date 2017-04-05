module Main where

import           Control.Monad       (when)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Plexams
import           Plexams.Export
import           Plexams.GUI
import           Plexams.Import
import           Plexams.PlanManip
import           Plexams.Types

data Config = Config
    { exportToMarkdown :: Bool
    , exportToHTML     :: Bool
    , planManipFile    :: Maybe FilePath
    }

config :: Parser Config
config = Config
    <$> switch
        ( long "toMd"
       <> short 'm'
       <> help "Plan in Markdown"
        )
    <*> switch
        ( long "toHtml"
       <> short 't'
       <> help "Plan as HTML table"
        )
    <*> optional (strOption
        ( long "planManip"
       <> short 'p'
       <> metavar "PLANMANIPFILE"
       <> help "file containing plan manipulations"
        )
        )

main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Tool for planning exams"
     <> header "plexams"
      )

main' :: Config -> IO ()
main' config =
  do
      maybeSemesterConfig <- initSemesterConfigFromFile "./plexams-config.json"
      maybeExams <- importExamsFromJSONFile "./initialplan.json"
      case maybeSemesterConfig of
          Nothing -> putStrLn "no semester config"
          Just semesterConfig -> do
              let -- emptyPlan = makeEmptyPlan semesterConfig
                  plan' = makePlan (maybe [] id maybeExams) semesterConfig Nothing
              plan <- maybe (return plan') (applyPlanManipToPlanWithFile plan')
                             $ planManipFile config
              when (exportToMarkdown config) $ putStrLn $ planToMD plan
              when (exportToHTML config)     $ putStrLn $ planToHTMLTable plan
      -- mainGUI

