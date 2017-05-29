module Main where

import           Control.Monad                (unless, when)
import           Data.List                    (intercalate)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.Semigroup               ((<>))
import           Options.Applicative
import           Plexams.Export.HTML
import           Plexams.Export.Markdown
import           Plexams.Export.Misc
import           Plexams.Export.ZPA
import qualified Plexams.Generators.Rooms
import           Plexams.Generators.Schedule
import           Plexams.GUI
import           Plexams.Import.MasterData
import           Plexams.Import.PlanManip
import           Plexams.Import.Registrations
import           Plexams.PlanManip
import           Plexams.Query
import           Plexams.Statistics
import           Plexams.Types
import qualified Plexams.Validation           as P (validate, validateZPAExport)
import           System.Directory             (doesFileExist)
import           System.IO                    (hPutStrLn, stderr)

data Command
    = Markdown
    | HTML { showConflictsAncodes :: Maybe [Integer]
           }
    | Statistics { initialStatistics :: Bool
                 }
    | Dot { groupDependencies :: Bool
          }
    | Validate
    | Query { byAncode        :: Maybe Integer
            , byName          :: Maybe String
            , byLecturer      :: Maybe String
            , byGroup         :: Maybe String
            , slot            :: Maybe (Int, Int)
            , onlyUnscheduled :: Bool
            }
    | ExportZPA
    | PrintConfig
    | Generate { scheduleSameNames :: Bool
               }
    | GenerateRooms
  deriving (Eq)

data Config = Config
    { optCommand      :: Command
    , planManipFile'  :: Maybe FilePath
    , regsFile        :: Maybe FilePath
    , overlapsFile    :: Maybe FilePath
    , constraintsFile :: Maybe FilePath
    , studentsFile    :: Maybe FilePath
    , outfile         :: Maybe FilePath
    , configfile      :: FilePath
    , novalidation    :: Bool
    }

config :: Parser Config
config = Config
    <$> hsubparser
          ( command "markdown"    (info (pure Markdown)
                                  (progDesc "the plan as markdown document"))
         <> command "html"      (info htmlOpts
                                 (progDesc "the plan as an HTML table"))
         <> command "stats"     (info statisticsOpts
                                  (progDesc "statistics"))
         <> command "validate"  (info (pure Validate)
                                  (progDesc "validation of current plan"))
         <> command "query"     (info  queryOpts
                                  (progDesc "query plan"))
         <> command "exportzpa" (info  (pure ExportZPA)
                                  (progDesc "export current plan for ZPA"))
         <> command "config"    (info  (pure PrintConfig)
                                  (progDesc "print the current config"))
         <> command "generate"  (info generateOpts
                                  (progDesc "generate part of the plan"))
         <> command "generate-rooms"  (info (pure GenerateRooms)
                                  (progDesc "generate rooms for the schedule"))
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
        ( long "overlaps"
       <> short 'l'
       <> metavar "OVERLAPSFILE"
       <> help "import file containing overlaps"
        ))
      <*> optional (strOption
        ( long "constraints"
       <> short 'c'
       <> metavar "CONSTRAINTSFILE"
       <> help "import file containing constraints"
        ))
      <*> optional (strOption
        ( long "students"
       <> short 's'
       <> metavar "STUDENTSFILE"
       <> help "import file containing registrations for each mtknr"
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

htmlOpts :: Parser Command
htmlOpts = HTML
    <$> optional (option auto
        ( long "ancodes"
       <> short 'a'
       <> metavar "LISTOFANCODES"
       <> help "show conflicts for given ancodes"
        ))

queryOpts :: Parser Command
queryOpts = Query
    <$> optional (option auto
        ( long "ancode"
       <> short 'a'
       <> metavar "EXAMID"
       <> help "query by exam id"
        ))
    <*> optional (strOption
        ( long "name"
       <> short 'n'
       <> metavar "NAME"
       <> help "query by name (substring of name)"
        ))
    <*> optional (strOption
        ( long "lecturer"
       <> short 'l'
       <> metavar "LECTURERID"
       <> help "query by lecturer name (substring of name)"
        ))
    <*> optional (strOption
        ( long "group"
       <> short 'g'
       <> metavar "GROUP"
       <> help "query by group"
        ))
  <*> optional (option auto
      ( long "slot"
     <> short 's'
     <> metavar "(DAYINDEX,SLOTINDEX)"
     <> help "query the slot"
      ))
    <*> switch
        ( long "unscheduled-only"
       <> short 'u'
       <> help "show only unscheduled"
        )

statisticsOpts :: Parser Command
statisticsOpts = Statistics
    <$> switch
        ( long "initial"
       <> short 'i'
       <> help "statistics for initial plan"
        )

generateOpts :: Parser Command
generateOpts = Generate
    <$> switch
        ( long "schedule-same-name"
       <> short 'n'
       <> help ("schedule unscheduled exams with same name "
                ++ " than the ONE already scheduled")
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
  maybeSemesterConfig <- importSemesterConfigFromYAMLFile $ configfile config
  case maybeSemesterConfig of
    Nothing -> putStrLn "no semester config"
    Just semesterConfig -> do
      -- read exams from file
      maybeExams <- importExamsFromJSONFile $ initialPlanFile semesterConfig
      -- maybe read registrations from file
      maybeRegs <- maybe (return Nothing) importRegistrationsFromYAMLFile
                              $ regsFile config
      maybeStuds <- maybe (return Nothing) importStudentsFromYAMLFile
                              $ studentsFile config
      let exams = fromMaybe [] maybeExams
          examsWithRegs = maybe exams
                                (addRegistrationsListToExams exams) maybeRegs
      when (isJust maybeRegs) $ hPutStrLn stderr ">>> Adding registrations"
      maybeOverlaps <- maybe (return Nothing) importOverlapsFromYAMLFile
                              $ overlapsFile config
      when (isJust maybeOverlaps) $ hPutStrLn stderr ">>> Adding overlaps"
      maybeConstraints <- maybe (return Nothing) importConstraintsFromYAMLFile
                                $ constraintsFile config
      when (isJust maybeConstraints) $ hPutStrLn stderr ">>> Adding constraints"
      -- generate initial plan
      let constraints' = fromMaybe noConstraints maybeConstraints
          constraints = constraints' { overlaps = fromMaybe [] maybeOverlaps }
          plan'' = makePlan examsWithRegs semesterConfig Nothing maybeStuds
          plan' = addConstraints plan'' constraints
      -- maybe manipulate the plan
      plan <- maybe (applyFileFromConfig plan' (planManipFile semesterConfig))
                    (applyAddExamToPlanWithFile plan')
                     $ planManipFile' config
      -- call the command function
      commandFun (optCommand config) config  plan
      when (optCommand config /= Validate) $
        hPutStrLn stderr $ if novalidation config
          then ">>> Validation off"
          else case fst $ P.validate plan of
            EverythingOk          -> ">>> Validation ok!"
            SoftConstraintsBroken -> ">>> Validation: Soft Constraints broken\n"
            HardConstraintsBroken -> ">>> Validation: Hard Constraints broken\n"

commandFun :: Command -> (Config -> Plan -> IO ())
commandFun Markdown      = markdown
commandFun HTML {}       = html
commandFun Statistics {} = stats
commandFun Validate      = validate
commandFun Query {}      = query
commandFun ExportZPA {}  = exportZPA
commandFun PrintConfig   = printConfig
commandFun Generate {}   = generate
commandFun GenerateRooms = generateRooms

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
    maybe (putStrLn output) (`writeFile` output) $ outfile config

markdown :: Config -> Plan -> IO ()
markdown config = stdoutOrFile config . planToMD

html :: Config -> Plan -> IO ()
html config = stdoutOrFile config
            . planToHTMLTable (showConflictsAncodes $ optCommand config)

stats :: Config -> Plan -> IO ()
stats config plan =
  let initialStats = case optCommand config of
          Statistics True -> initialPlanStatistics plan
          _               -> ""
      currentStats = planStatistics plan
  in  stdoutOrFile config $ initialStats ++ currentStats

validate :: Config -> Plan -> IO ()
validate config = stdoutOrFile config . validate'
  where
    validate' plan =
      let (ok, msgs) = P.validate plan
      in "\n# " ++ show ok ++ "\n\n" ++ intercalate "\n\n" msgs

query :: Config -> Plan -> IO ()
query config plan = stdoutOrFile config
    $ intercalate "\n" $ map show $ query' (optCommand config)
  where
    query' (Query (Just a) _ _ _ _ _) = queryByAnCode a plan
    query' (Query _ (Just n) _ _ _ _) = queryByName n plan
    query' (Query _ _ (Just l) _ _ _) = queryByLecturer l plan
    query' (Query _ _ _ (Just g) _ u) = queryByGroup g u plan
    query' (Query _ _ _ _ (Just s) _) = querySlot s plan
    query' _                          = []

exportZPA :: Config -> Plan -> IO ()
exportZPA config plan = do
  stdoutOrFile config $ planToZPA plan
  case outfile config of
    Nothing -> return ()
    Just fp -> do
      (valRes, msgs) <- P.validateZPAExport fp plan
      putStrLn $ intercalate "\n\n" msgs
      print valRes

printConfig :: Config -> Plan -> IO ()
printConfig config = stdoutOrFile config . semesterConfigAsString

applyFileFromConfig :: Plan -> FilePath -> IO Plan
applyFileFromConfig plan file = do
    fileExist <- doesFileExist file
    if fileExist
      then applyAddExamToPlanWithFile plan file
      else return plan

generate :: Config -> Plan -> IO ()
generate config plan =
  stdoutOrFile config $ generate' (optCommand config)
    where
      generate' (Generate True) =
        ("# Slots for exams with same name --- generated\n"++)
        $ exportAddExamToSlots
        $ snd
        $ scheduleExamsWithSameName plan

generateRooms :: Config -> Plan -> IO ()
generateRooms config plan =
  stdoutOrFile config generateRooms'
    where
      generateRooms'  =
        ("# Rooms for exams --- generated\n"++)
        $ exportAddRoomToExams
        -- $ intercalate "\n"
        -- $ map show
        $ snd
        $ Plexams.Generators.Rooms.generateRooms plan
        -- $ map (\(s,(nrs,hrs)) -> show (s, ( map availableRoomName nrs
        --                                   , map availableRoomName hrs)))
        -- $ M.toList
        -- $ mkAvailableRooms plan
        -- $ availableRooms
        -- $ semesterConfig plan
        -- $ maybe M.empty roomSlots $ constraints plan

applyAddExamToPlanWithFile :: Plan -> FilePath -> IO Plan
applyAddExamToPlanWithFile plan filePath =
  fmap (maybe plan (applyAddExamToSlotListToPlan plan))
       (importExamSlotsFromYAMLFile filePath)
