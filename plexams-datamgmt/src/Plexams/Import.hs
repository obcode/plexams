{-# LANGUAGE OverloadedStrings #-}

module Plexams.Import
  ( setPlexamsDirectory
  , importPlan
  , importExamSlotsFromYAMLFile
  , importAddInvigilatorToRoomOrSlotFromYAMLFile
  , importZPAExamsFromJSONFile
  )
where

import           Control.Monad.Trans            ( liftIO )
import           Control.Monad.Writer           ( WriterT
                                                , runWriterT
                                                , tell
                                                )
import qualified Data.Map                      as M
import           Data.Text                      ( Text
                                                , append
                                                , pack
                                                )
import           System.Directory               ( getHomeDirectory
                                                , setCurrentDirectory
                                                , doesFileExist
                                                )

import           Plexams.Import.MasterData
import           Plexams.Import.Misc
import           Plexams.Import.PlanManip
import           Plexams.Import.Registrations
import           Plexams.Invigilation
import           Plexams.PlanManip
import           Plexams.Types

plexamsrc :: FilePath
plexamsrc = ".plexamsrc"

setPlexamsDirectory :: IO ()
setPlexamsDirectory = do
  plexamsYamlExists <- doesFileExist "plexams.yaml"
  if plexamsYamlExists
    then putStrLn "INFO: using config in current directory"
    else do
      homedir <- getHomeDirectory
      dir     <- fmap (head . lines) $ readFile $ homedir ++ "/" ++ plexamsrc
      putStrLn $ "INFO: setting working directory to: " ++ dir
      setCurrentDirectory dir

semesterConfigFile :: FilePath
semesterConfigFile = "plexams.yaml"

importPlan :: IO (Maybe Plan, [Text]) -- Error-Messages
importPlan = runWriterT importPlanWriter

importPlanWriter :: WriterT [Text] IO (Maybe Plan)
importPlanWriter = do
  semesterConfig' <- liftIO
    $ importSemesterConfigFromYAMLFile semesterConfigFile
  importPlan' semesterConfig'

importPlan' :: SemesterConfig -> WriterT [Text] IO (Maybe Plan)
importPlan' semesterConfig' = do
  let files' = files semesterConfig'
  -- load exams
  exams' <-
    maybe (tell ["WARNING: initial plan not readable"] >> return [])
          (importFromFilePath importExamsFromJSONFile [])
      $ initialPlanFile files'
  -- load persons == lecturers
  persons' <-
    maybe (tell ["WARNING: persons file not readable"] >> return M.empty)
          (importFromFilePath importPersonsFromJSONFile M.empty)
      $ personsFile files'
  -- make the initial plan
  -- add constraints
  constraints' <-
    maybe
        (tell ["WARNING: constraints file not readable"] >> return noConstraints
        )
        (importFromFilePath (importConstraintsFromYAMLFile semesterConfig')
                            noConstraints
        )
      $ constraintsFile files'
  -- let planWithConstraints = addConstraints constraints' plan'
  let plan' = makePlan exams' semesterConfig' persons' constraints'
  -- add students registrations
  studentWithRegs' <-
    maybe
        (tell ["WARNING: student regs file not readable"] >> return M.empty)
        (importFromFilePath
          (importStudentsWithRegsFromYAMLFile semesterConfig')
          M.empty
        )
      $ studentsRegsFile files'
  -- and add handicaps
  handicaps' <-
    maybe (tell ["WARNING: handicaps file not readable"] >> return [])
          (importFromFilePath importHandicapsFromYAMLFile [])
      $ handicapsFile files'
  let studentWithRegs'' = addHandicaps studentWithRegs' handicaps'
      planWithRegs      = addStudentRegistrationsToPlan studentWithRegs'' plan'
  -- let planWithHandicaps = addHandicaps handicaps' planWithRegs
  -- add PlanManip ExamToSlot
  addExamsToSlots <-
    maybe (tell ["WARNING: planmanip file not readable"] >> return [])
          (importFromFilePath importExamSlotsFromYAMLFile [])
      $ planManipFile files'
  let planWithExamsInSlots =
        applyAddExamToSlotListToPlan planWithRegs addExamsToSlots
  -- add rooms to exams
  rooms' <-
    maybe (tell ["WARNING: rooms file not readable"] >> return [])
          (importFromFilePath importAddRoomToExamFromYAMLFile [])
      $ roomsFile files'
  let planWithRooms = applyAddRoomToExamListToPlan planWithExamsInSlots rooms'
  -- add invigilators
  invigilators' <-
    maybe (tell ["WARNING: invigilators file not readable"] >> return [])
          (importFromFilePath importInvigilatorsFromJSONFile [])
      $ invigilatorsFile files'
  let planWithInvigilators = addInvigilators invigilators' planWithRooms
  -- add invigilations
  invigilations' <-
    maybe (tell ["WARNING: invigilations file not readable"] >> return [])
          (importFromFilePath importAddInvigilatorToRoomOrSlotFromYAMLFile [])
      $ invigilationsFile files'
  let planWithInvigilations =
        applyAddInvigilatorsToPlan planWithInvigilators invigilations'
  return $ Just planWithInvigilations

importFromFilePath
  :: (FilePath -> IO (Maybe a)) -> a -> FilePath -> WriterT [Text] IO a
importFromFilePath importer errorResult fp = do
  maybeResult <- liftIO $ importer fp
  maybe
    (do
      tell ["WARNING: " `append` pack fp `append` " is not parsable."]
      return errorResult
    )
    (\r -> tell ["INFO: " `append` pack fp `append` " imported."] >> return r)
    maybeResult
  -- TODO: if file does not exist
