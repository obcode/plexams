module Plexams.Server.Import where

import           Plexams.Import.MasterData
import           Plexams.Import.Registrations
import           Plexams.PlanManip
import           Plexams.Types
import           System.Directory
import           System.Exit
import           System.IO                    (hPutStrLn, stderr)

semesterConfig' :: IO (Either String SemesterConfig)
semesterConfig' = importSemesterConfig configFile'

configFile' :: FilePath
configFile' = "plexams.yaml"

overlapsFile :: FilePath
overlapsFile = "input/overlaps.yaml"

constraintsFile :: FilePath
constraintsFile = "input/constraints.yaml"

studentsFile' :: FilePath
studentsFile' = "input/students.yaml"

handicapsFile' :: FilePath
handicapsFile' = "input/handicaps.yaml"

planManipFile' :: FilePath
planManipFile' = "input/planmanip.yaml"

exportTestFile :: FilePath
exportTestFile = "input/exportTest.yaml"

importSemesterConfig :: FilePath -> IO (Either String SemesterConfig)
importSemesterConfig configfile = do
  maybeSemesterConfig <- importSemesterConfigFromYAMLFile configfile
  case maybeSemesterConfig of
    Just semesterConfig'' -> return (Right semesterConfig'')
    Nothing               -> return (Left "SemesterConfig import failed.")

importExams :: SemesterConfig -> IO (Either String [Exam])
importExams semesterConfig'' = do
  maybeExams <- importExamsFromJSONFile $ initialPlanFile semesterConfig''
  case maybeExams of
    Just exams' -> return (Right exams')
    Nothing     -> return (Left (initialPlanFile semesterConfig''
                              ++ " does not exist or is not parsable."))

importAndAddRegs :: Maybe FilePath -> [Exam] -> IO [Exam]
importAndAddRegs regsFile exams' =
  case regsFile of
    Just file -> do
      maybeRegs <- importRegistrationsFromYAMLFile file
      case maybeRegs of
        Just regs' -> do
          hPutStrLn stderr ">>> Adding registrations"
          return $ addRegistrationsListToExams exams' regs'
        Nothing ->  do
          hPutStrLn stderr $ "no registration found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 3
    Nothing -> do
      hPutStrLn stderr "no registration file specified"
      return exams'

importStudents :: FilePath -> IO (Maybe Students)
importStudents studentsFile = do
  exists <- doesFileExist studentsFile
  if (exists)
    then do
      maybeStuds <- importStudentsFromYAMLFile studentsFile
      case maybeStuds of
        Just _ -> do
          hPutStrLn stderr ">>> importing students"
          return maybeStuds
        Nothing -> do
          hPutStrLn stderr $ "no students found: "
                            ++ studentsFile
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 4
    else do
      print $"no students file found at " ++ studentsFile
      return Nothing


importConstraints :: Maybe FilePath -> Maybe FilePath -> IO (Either String Constraints)
importConstraints overlapsFile' constraintsFile' = do
  overlaps' <- importOverlaps overlapsFile'
  case overlaps' of
    Left errorMsg -> return $ Left errorMsg
    Right overlaps'' -> do
      constraints' <- importConstraints' constraintsFile'
      case constraints' of
        Left errorMsg -> return $ Left errorMsg
        Right constraints'' -> return $ Right $ constraints'' { overlaps = overlaps'' }

importConstraints' :: Maybe FilePath -> IO (Either String Constraints)
importConstraints' constraintsFile' =
  case constraintsFile' of
    Just file -> do
      maybeConstraints <- importConstraintsFromYAMLFile file
      case maybeConstraints of
        Just constraints' -> do
          hPutStrLn stderr ">>> importing constraints"
          return $ Right constraints'
        Nothing -> do
          hPutStrLn stderr $ "no constraints found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          return $ Left $ "no contraints found: " ++ file
    Nothing -> do
      hPutStrLn stderr "no constraints file specified"
      return $ Left "no contraints file found"

importOverlaps :: Maybe FilePath -> IO (Either String [Overlaps])
importOverlaps overlapsFile' =
  case overlapsFile' of
    Just file -> do
      maybeOverlaps <- importOverlapsFromYAMLFile file
      case maybeOverlaps of
        Just overlaps' -> do
          hPutStrLn stderr ">>> importing overlaps"
          return $ Right overlaps'
        Nothing -> do
          hPutStrLn stderr $ "no overlaps found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          return $ Left $ "not overlaps found: " ++ file
    Nothing -> do
      hPutStrLn stderr "no overlaps file specified"
      return $ Left "no overlaps file found"

importHandicaps :: Maybe FilePath -> IO [Handicap]
importHandicaps handicapFile =
  case handicapFile of
    Just file -> do
      maybeHandicaps <- importHandicapsFromYAMLFile file
      case maybeHandicaps of
        Just handicaps' -> do
          hPutStrLn stderr ">>> importing handicaps"
          return handicaps'
        Nothing -> do
          hPutStrLn stderr $ "no handicaps found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 8
    Nothing -> do
      hPutStrLn stderr "no handicaps file specified"
      return []

importPersons :: SemesterConfig -> IO (Either String Persons)
importPersons semesterConfig'' = do
  maybePersons <- importPersonsFromJSONFile $ personsFile semesterConfig''
  case maybePersons of
    Just persons' -> return (Right persons')
    Nothing       -> return (Left (personsFile semesterConfig''
                              ++ " does not exist or is not parsable."))

importInvigilators :: Maybe FilePath -> IO [Invigilator]
importInvigilators invigilatorFile =
  case invigilatorFile of
    Just file -> do
      maybeInvigilators <- importInvigilatorsFromJSONFile file
      case maybeInvigilators of
        Just invigilators' -> do
          hPutStrLn stderr ">>> importing invigilators"
          return invigilators'
        Nothing -> do
          hPutStrLn stderr $ "no invigilators found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 11
    Nothing -> do
      hPutStrLn stderr "no invigilators file specified"
      return []
