{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import
  ( importPlan
  , module Plexams.Import.MasterData
  , module Plexams.Import.Misc
  , module Plexams.Import.PlanManip
  , module Plexams.Import.Registrations
  ) where

import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Writer         (WriterT, runWriterT, tell)
import qualified Data.Map                     as M
import           Data.Text                    (Text, append, pack)
import           Plexams.Import.MasterData
import           Plexams.Import.Misc
import           Plexams.Import.PlanManip
import           Plexams.Import.Registrations
import           Plexams.PlanManip
import           Plexams.Types

semesterConfigFile :: FilePath
semesterConfigFile = "plexams.yaml"

importPlan :: IO (Maybe Plan, [Text]) -- Error-Messages
importPlan = runWriterT importPlanWriter

importPlanWriter :: WriterT [Text] IO (Maybe Plan)
importPlanWriter = do
  maybeSemesterConfig <- liftIO $ importSemesterConfigFromYAMLFile semesterConfigFile
  case maybeSemesterConfig of
    Nothing -> do
      tell ["Semesterconfig file `" `append` pack semesterConfigFile `append` "` not found"]
      return Nothing
    Just semesterConfig' ->
      importPlan' semesterConfig'

importPlan' :: SemesterConfig -> WriterT [Text] IO (Maybe Plan)
importPlan' semesterConfig' = do
  let files' = files semesterConfig'
  exams' <- maybe (tell ["initial plan can not be read"] >> return [])
                  (importFromFilePath importExamsFromJSONFile [])
                  $ initialPlanFile files'
  persons' <- maybe (tell ["persons file can not be read"] >> return M.empty)
                    (importFromFilePath importPersonsFromJSONFile M.empty)
                    $ personsFile files'
  let maybeStudents = Nothing -- TODO: still needed?
  handicaps' <- maybe (tell ["handicaps file can not be read"] >> return [])
                (importFromFilePath importHandicapsFromYAMLFile [])
                $ handicapsFile files'
  let plan' = makePlan exams' semesterConfig' persons' maybeStudents handicaps'
  studentWithRegs' <- maybe (tell ["student regs file can not be read"] >> return M.empty)
    (importFromFilePath (importStudentsWithRegsFromYAMLFile semesterConfig') M.empty)
    $ studentsRegsFile files'
  let planWithRegs = addStudentRegistrationsToPlan studentWithRegs' plan'
  addExamsToSlots <- maybe (tell ["planmanip file can not be read"] >> return [])
                           (importFromFilePath importExamSlotsFromYAMLFile [])
                           $ planManipFile files'
  let planWithExamsInSlots = applyAddExamToSlotListToPlan planWithRegs addExamsToSlots
  tell ["constraints file missing"]
  tell ["rooms file missing"]
  tell ["invigilators file missing"]
  return $ Just planWithExamsInSlots

importFromFilePath :: (FilePath -> IO (Maybe a)) -> a -> FilePath -> WriterT [Text] IO a
importFromFilePath importer errorResult fp = do
  maybeResult <- liftIO $ importer fp
  maybe (do
          tell ["warning: " `append` pack fp `append` " is not parsable."]
          return errorResult
        )
        return maybeResult


{-
importPersons :: FilePath -> WriterT [Text] IO Persons
importPersons filepath = do
  maybePersons <- liftIO $ importPersonsFromJSONFile filepath
  case maybePersons of
    Just persons' -> return persons'
    Nothing             -> do
      hPutStrLn stderr $ "no initial persons found: "
                        ++ personsFile semesterConfig'
                        ++ " does not exist or is not parsable."
      exitWith $ ExitFailure 10



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
-}
