module Main where

import           Control.Monad       (when)
import           Data.List           (intercalate)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Plexams
import           Plexams.Export
import           Plexams.GUI
import           Plexams.Import
import           Plexams.PlanManip
import           Plexams.Query
import           Plexams.Statistics
import           Plexams.Types
import           Plexams.Validation
import           System.Directory    (doesFileExist)

data Command
    = Markdown
    | HTML
    | Statistics
    | Validate
    | Query { byAncode        :: Maybe Integer
            , byGroup         :: Maybe String
            , onlyUnscheduled :: Bool
            }
    | ExportZPA { zpafile :: FilePath }
  deriving (Eq)

data Config = Config
    { optCommand     :: Command
    , planManipFile' :: Maybe FilePath
    , outfile        :: Maybe FilePath
    , configfile     :: FilePath
    , novalidation   :: Bool
    }

config :: Parser Config
config = Config
    <$> hsubparser
          ( command "html"      (info (pure HTML)       (progDesc "the plan as an HTML table"))
         <> command "stats"     (info (pure Statistics) (progDesc "statistics"))
         <> command "validate"  (info (pure Validate)   (progDesc "validation of current plan"))
         <> command "query"     (info  queryOpts        (progDesc "query plan"))
         <> command "zpa"       (info  exportZPAOpts    (progDesc "export current plan for ZPA"))
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
    <*> strOption
        ( long "config"
       <> short 'c'
       <> showDefault
       <> value "plexams.yaml"
       <> metavar "CONFIGFILE"
       <> help "file containing semesterconfig"
        )
    <*> switch
        ( long "no-validation"
       <> help "turn of validation"
        )

queryOpts :: Parser Command
queryOpts = Query
    <$> optional (option auto
        ( long "ancode"
       <> metavar "EXAMID"
       <> help "query by exam id"
        ))
    <*> optional (strOption
        ( long "group"
       <> metavar "GROUP"
       <> help "query by group"
        ))
    <*> switch
        ( long "unscheduled-only"
       <> short 'u'
       <> help "show only unscheduled"
        )

exportZPAOpts :: Parser Command
exportZPAOpts = ExportZPA
    <$> strOption
        ( long "file"
       <> short 'f'
       <> metavar "FILE"
       <> help "output in JSON file"
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
main' config = do
    maybeSemesterConfig <- initSemesterConfigFromFile $ configfile config
    case maybeSemesterConfig of
        Nothing -> putStrLn "no semester config"
        Just semesterConfig -> do
            maybeExams <- importExamsFromJSONFile $ initialPlanFile semesterConfig
            -- generate initial plan
            let plan' = makePlan (maybe [] id maybeExams) semesterConfig Nothing
            -- maybe manipulate the plan
            plan <- maybe (applyFileFromConfig plan' (planManipFile semesterConfig))
                          (applyPlanManipToPlanWithFile plan')
                           $ planManipFile' config
            -- output in Markdown or HTML
            let output = case optCommand config of
                            Markdown    -> planToMD plan
                            HTML        -> planToHTMLTable plan
                            Statistics  -> planStatistics plan
                            Validate    -> show $ validatePlan plan
                            Query mA mG un -> intercalate "\n" $ map show $ query mA mG un plan
            maybe (putStrLn output) (flip writeFile output) $ outfile config
            when (optCommand config /= Validate) $
              putStrLn $ if novalidation config
                then ">>> Validation off"
                else show $ validatePlan plan

query :: Maybe Integer -> Maybe String -> Bool -> Plan -> [Exam]
query mA mG un plan = case mA of
    Just a  -> queryByAnCode a plan
    Nothing -> case mG of
        Just g  -> queryByGroup g un plan
        Nothing -> []

applyFileFromConfig :: Plan -> FilePath -> IO Plan
applyFileFromConfig plan file = do
    fileExist <- doesFileExist file
    if fileExist
      then applyPlanManipToPlanWithFile plan file
      else return plan

