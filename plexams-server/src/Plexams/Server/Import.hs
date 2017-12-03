module Plexams.Server.Import where

import           Plexams.Import.MasterData
import           Plexams.Import.Registrations
import           Plexams.Types
import           System.IO.Error

-- TODO: refactor

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

studentRegsFile :: FilePath
studentRegsFile = "input/studentregs.yaml"

handicapsFile' :: FilePath
handicapsFile' = "input/handicaps.yaml"

planManipFile' :: FilePath
planManipFile' = "input/planmanip.yaml"

exportTestFile :: FilePath
exportTestFile = "input/exportTest.yaml"

importSemesterConfig :: FilePath -> IO (Either String SemesterConfig)
importSemesterConfig configfile =
  importFile configfile importSemesterConfigFromYAMLFile

importExams :: SemesterConfig -> IO (Either String [Exam])
importExams semesterConfig'' =
  importFile (initialPlanFile semesterConfig'') importExamsFromJSONFile

importStudents :: FilePath -> IO (Either String (Maybe Students))
importStudents studentsFile = do
  eitherStuds <- tryIOError $ importStudentsFromYAMLFile studentsFile
  case eitherStuds of
    Left _ -> return (Left (studentsFile ++ " import failed. File does not exist"))
    Right maybeStuds ->
      case maybeStuds of
        Just _ -> return $ Right maybeStuds
        Nothing -> return $ Left $ studentsFile
                                ++  " import failed."

importConstraints :: FilePath -> FilePath -> IO (Either String Constraints)
importConstraints overlapsFile' constraintsFile' = do
  eitherOverlaps' <- importOverlaps overlapsFile'
  case eitherOverlaps' of
    Left errorMsg -> return $ Left errorMsg
    Right overlaps'' -> do
      constraints' <- importConstraints' constraintsFile'
      case constraints' of
        Left errorMsg -> return $ Left errorMsg
        Right constraints'' -> return $ Right $ constraints'' { overlaps = overlaps'' }

importConstraints' :: FilePath -> IO (Either String Constraints)
importConstraints' constraintsFile' =
  importFile constraintsFile' importConstraintsFromYAMLFile

importOverlaps :: FilePath -> IO (Either String [Overlaps])
importOverlaps overlapsFile' =
  importFile overlapsFile' importOverlapsFromYAMLFile

importHandicaps :: FilePath -> IO (Either String [Handicap])
importHandicaps handicapFile =
  importFile handicapFile importHandicapsFromYAMLFile

importPersons :: SemesterConfig -> IO (Either String Persons)
importPersons semesterConfig'' =
   importFile (personsFile semesterConfig'') importPersonsFromJSONFile

importInvigilators :: FilePath -> IO (Either String [Invigilator])
importInvigilators invigilatorFile =
  importFile invigilatorFile importInvigilatorsFromJSONFile

importFile :: FilePath -> (FilePath -> IO (Maybe a)) -> IO (Either String a)
importFile filepath importFun= do
  eitherContent <- tryIOError $ importFun filepath
  case eitherContent of
    Left _ -> return $ Left $ filepath ++ " import failed. File does not exist"
    Right maybeContent ->
      case maybeContent of
        Just content -> return $ Right content
        Nothing      -> return $ Left $ filepath ++ " import failed."

registrationsFile :: FilePath
registrationsFile = "input/registrations.yaml"

importRegistrations :: IO (Maybe [Registrations])
importRegistrations = do
  Right semesterConfig'' <- semesterConfig'
  importRegistrationsFromYAMLFile semesterConfig'' registrationsFile

importStudentRegistrations :: IO (Maybe StudentsWithRegs)
importStudentRegistrations = do
  Right semesterConfig'' <- semesterConfig'
  importStudentsWithRegsFromYAMLFile semesterConfig'' studentRegsFile
