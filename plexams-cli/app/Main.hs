module Main where

import           Control.Monad       (when)
import           Data.List           (intercalate)
import           Data.Maybe          (fromMaybe)
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
    , regsFile       :: Maybe FilePath
    , outfile        :: Maybe FilePath
    , configfile     :: FilePath
    , novalidation   :: Bool
    }

config :: Parser Config
config = Config
    <$> hsubparser
          ( command "markdown"    (info (pure Markdown)
                                  (progDesc "the plan as markdown document"))
         <> command "html"      (info (pure HTML)
                                 (progDesc "the plan as an HTML table"))
         <> command "stats"     (info (pure Statistics)
                                  (progDesc "statistics"))
         <> command "validate"  (info (pure Validate)
                                  (progDesc "validation of current plan"))
         <> command "query"     (info  queryOpts
                                  (progDesc "query plan"))
         <> command "zpa"       (info  exportZPAOpts
                                  (progDesc "export current plan for ZPA"))
          )
    <*> optional (strOption
        ( long "planManip"
       <> short 'p'
       <> metavar "PLANMANIPFILE"
       <> help "import file containing plan manipulations"
        ))
        <*> optional (strOption
        ( long "registrations"
       <> short 'r'
       <> metavar "REGISTRATIONSFILE"
       <> help "import file containing registrations"
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
       <> short 'a'
       <> metavar "EXAMID"
       <> help "query by exam id"
        ))
    <*> optional (strOption
        ( long "group"
       <> short 'g'
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
      -- read exams from file
      maybeExams <- importExamsFromJSONFile $ initialPlanFile semesterConfig
      -- maybe read registrations from file
      maybeRegs <- maybe (return Nothing) importRegistrationsFromYAMLFile
                              $ regsFile config
      let exams = fromMaybe [] maybeExams
          examsWithRegs = maybe exams
                                (addRegistrationsListToExams exams) maybeRegs
      -- generate initial plan
      let plan' = makePlan examsWithRegs semesterConfig Nothing
      -- maybe manipulate the plan
      plan <- maybe (applyFileFromConfig plan' (planManipFile semesterConfig))
                    (applyPlanManipToPlanWithFile plan')
                     $ planManipFile' config
      -- call the command function
      commandFun (optCommand config) config  plan
      when (optCommand config /= Validate) $
        putStrLn $ if novalidation config
          then ">>> Validation off"
          else if fst $ validatePlan plan
               then ">>> Validation ok!"
               else ">>> Validation NOT ok!\n"
                 ++ "    See `plexams validate` for more information."

commandFun :: Command -> (Config -> Plan -> IO ())
commandFun Markdown   = markdown
commandFun HTML       = html
commandFun Statistics = stats
commandFun Validate   = validate
commandFun Query {}   = query

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
    maybe (putStrLn output) (`writeFile` output) $ outfile config

markdown :: Config -> Plan -> IO ()
markdown config = stdoutOrFile config . planToMD

html :: Config -> Plan -> IO ()
html config = stdoutOrFile config . planToHTMLTable

stats :: Config -> Plan -> IO ()
stats config = stdoutOrFile config . planStatistics

validate :: Config -> Plan -> IO ()
validate config = stdoutOrFile config . validatePlan'
  where
    validatePlan' plan =
      let (ok, msgs) = validatePlan plan
      in intercalate "\n" msgs
        ++ if ok then "\n# Validation successful."
                 else "\n# Validation failed."

query :: Config -> Plan -> IO ()
query config plan = stdoutOrFile config
    $ intercalate "\n" $ map show $ query' (optCommand config)
  where
    query' (Query (Just a) _ _) = queryByAnCode a plan
    query' (Query _ (Just g) u) = queryByGroup g u plan
    query' _                    = []

applyFileFromConfig :: Plan -> FilePath -> IO Plan
applyFileFromConfig plan file = do
    fileExist <- doesFileExist file
    if fileExist
      then applyPlanManipToPlanWithFile plan file
      else return plan
