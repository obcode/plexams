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

data OutputFormat = Markdown | HTML

data Config = Config
    { outputFormat  :: OutputFormat
    , planManipFile :: Maybe FilePath
    , outfile       :: Maybe FilePath
    }

config :: Parser Config
config = Config
    <$> (flag' Markdown
            ( long "markdown"
           <> help "markdown output"
            )
        <|>
         flag' HTML
            ( long "html"
           <> help "html output"
            )
        )
    <*> optional (strOption
        ( long "planManip"
       <> short 'p'
       <> metavar "PLANMANIPFILE"
       <> help "import file containing plan manipulations"
        ))
    <*> optional (strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTFILE"
       <> help "output to file instead of stdout"
        ))

main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Tool for planning exams"
     <> header "plexams"
      )

main' :: Config -> IO ()
main' config = do
    maybeSemesterConfig <- initSemesterConfigFromFile "./plexams-config.json"
    maybeExams <- importExamsFromJSONFile "./initialplan.json"
    case maybeSemesterConfig of
        Nothing -> putStrLn "no semester config"
        Just semesterConfig -> do
            -- generate initial plan
            let plan' = makePlan (maybe [] id maybeExams) semesterConfig Nothing
            -- maybe manipulate the plan
            plan <- maybe (return plan') (applyPlanManipToPlanWithFile plan')
                           $ planManipFile config
            -- output in Markdown or HTML
            let output = case outputFormat config of
                            Markdown -> planToMD plan
                            HTML -> planToHTMLTable plan
            maybe (putStrLn output) (flip writeFile output) $ outfile config
    -- mainGUI

