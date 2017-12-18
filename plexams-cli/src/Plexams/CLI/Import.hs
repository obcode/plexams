module Plexams.CLI.Import
  ( importSemesterConfig
  , importExams
  , importPersons
  , importAndAddRegs
  , importStudents
  , importStudentRegs
  , importConstraints
  , importHandicaps
  , importInvigilators
  ) where

import           Plexams.CLI.Types
import           Plexams.Import.MasterData
import           Plexams.Import.Registrations
import           Plexams.PlanManip
import           Plexams.Types                hiding (importExams)
import           System.Exit
import           System.IO                    (hPutStrLn, stderr)

importSemesterConfig :: Config -> IO SemesterConfig
importSemesterConfig config = do
  maybeSemesterConfig <- importSemesterConfigFromYAMLFile $ configfile config
  case maybeSemesterConfig of
    Just semesterConfig' -> return semesterConfig'
    Nothing             -> do
      hPutStrLn stderr $ "no config found: " ++ configfile config
                        ++ " does not exist or is not parsable."
      exitWith $ ExitFailure 1

importExams :: SemesterConfig -> IO [Exam]
importExams semesterConfig' = do
  maybeExams <- importExamsFromJSONFile $ initialPlanFile semesterConfig'
  case maybeExams of
    Just exams' -> return exams'
    Nothing             -> do
      hPutStrLn stderr $ "no initial exams found: "
                        ++ initialPlanFile semesterConfig'
                        ++ " does not exist or is not parsable."
      exitWith $ ExitFailure 2

importAndAddRegs :: Config -> SemesterConfig -> [Exam] -> IO [Exam]
importAndAddRegs config semesterConfig' exams' =
  case regsFile config of
    Just file -> do
      maybeRegs <- importRegistrationsFromYAMLFile semesterConfig' file
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

importStudents :: Config -> IO (Maybe Students)
importStudents config =
  case studentsFile config of
    Just file -> do
      maybeStuds <- importStudentsFromYAMLFile file
      case maybeStuds of
        Just _ -> do
          hPutStrLn stderr ">>> importing students"
          return maybeStuds
        Nothing -> do
          hPutStrLn stderr $ "no students found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 4
    Nothing -> do
      hPutStrLn stderr "no students file specified"
      return Nothing

importStudentRegs :: Config -> SemesterConfig -> IO (Maybe StudentsWithRegs)
importStudentRegs config semesterConfig' =
  case studentRegsFile config of
    Just file -> do
      maybeStuds <- importStudentsWithRegsFromYAMLFile semesterConfig' file
      case maybeStuds of
        Just _ -> do
          hPutStrLn stderr ">>> importing students with regs"
          return maybeStuds
        Nothing -> do
          hPutStrLn stderr $ "no students with regs found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 5
    Nothing -> do
      hPutStrLn stderr "no students with regs file specified"
      return Nothing

importConstraints :: Config -> IO Constraints
importConstraints config = do
  overlaps' <- importOverlaps config
  constraints' <- importConstraints' config
  return constraints' { overlaps = overlaps' }

importConstraints' :: Config -> IO Constraints
importConstraints' config =
  case constraintsFile config of
    Just file -> do
      maybeConstraints <- importConstraintsFromYAMLFile file
      case maybeConstraints of
        Just constraints' -> do
          hPutStrLn stderr ">>> importing constraints"
          return constraints'
        Nothing -> do
          hPutStrLn stderr $ "no constraints found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 6
    Nothing -> do
      hPutStrLn stderr "no constraints file specified"
      return noConstraints

importOverlaps :: Config -> IO [Overlaps]
importOverlaps config =
  case overlapsFile config of
    Just file -> do
      maybeOverlaps <- importOverlapsFromYAMLFile file
      case maybeOverlaps of
        Just overlaps' -> do
          hPutStrLn stderr ">>> importing overlaps"
          return overlaps'
        Nothing -> do
          hPutStrLn stderr $ "no overlaps found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 5
    Nothing -> do
      hPutStrLn stderr "no overlaps file specified"
      return []

importHandicaps :: Config -> IO [Handicap]
importHandicaps config =
  case handicapFile config of
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

importPersons :: SemesterConfig -> IO Persons
importPersons semesterConfig' = do
  maybePersons <- importPersonsFromJSONFile $ personsFile semesterConfig'
  case maybePersons of
    Just persons' -> return persons'
    Nothing             -> do
      hPutStrLn stderr $ "no initial persons found: "
                        ++ personsFile semesterConfig'
                        ++ " does not exist or is not parsable."
      exitWith $ ExitFailure 10

importInvigilators :: Config -> IO [Invigilator]
importInvigilators config =
  case invigilatorFile config of
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
