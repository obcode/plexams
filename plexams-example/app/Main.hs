module Main where

import           Control.Monad       (when)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Plexams
import           Plexams.Export
import           Plexams.GUI
import           Plexams.Import
import           Plexams.Types

data Config = Config
    { exportToMarkdown :: Bool
    , exportToHTML     :: Bool
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
       <> short 'h'
       <> help "Plan in HTML"
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
              let emptyPlan = makeEmptyPlan semesterConfig
                  plan = maybe emptyPlan (addUnscheduledExams emptyPlan) maybeExams
              when (exportToMarkdown config) $ putStrLn $ planToMD plan
              when (exportToHTML config)     $ putStrLn $ planToHTMLTable plan
      -- mainGUI
