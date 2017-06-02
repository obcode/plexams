module Plexams.CLI.PlanManip
  ( makePlan
  , applyPlanManips
  , applyAddRooms
  ) where

import           Plexams.CLI.Import
import           Plexams.CLI.Types
import           Plexams.Import.PlanManip
import           Plexams.PlanManip        hiding (makePlan)
import qualified Plexams.PlanManip
import           Plexams.Types
import           System.Directory         (doesFileExist)
import           System.Exit
import           System.IO                (hPutStrLn, stderr)

makePlan :: Config -> IO Plan
makePlan config = do
    semesterConfig <- importSemesterConfig config
    exams          <- importExams semesterConfig
    examsWithRegs  <- importAndAddRegs config semesterConfig exams
    maybeStudents  <- importStudents config
    constraints    <- importConstraints config
    handicaps      <- importHandicaps config

    return $ addConstraints constraints
           $ Plexams.PlanManip.makePlan examsWithRegs
                                        semesterConfig
                                        Nothing
                                        maybeStudents
                                        handicaps

applyPlanManips :: Config -> Plan -> IO Plan
applyPlanManips config plan =
  case planManipFile' config of
    Just file -> applyPlanManips' file plan
    Nothing -> do
      let file = planManipFile (semesterConfig plan)
      fileExist <- doesFileExist file
      if fileExist
        then applyPlanManips' file plan
        else do
          hPutStrLn stderr $ "planmanip file " ++ file ++ " not found"
          return plan

applyPlanManips' :: FilePath -> Plan -> IO Plan
applyPlanManips' file plan = do
  maybeExamSlots <- importExamSlotsFromYAMLFile file
  case maybeExamSlots of
    Just examSlots -> do
      hPutStrLn stderr ">>> applying plan manipulations"
      return $ applyAddExamToSlotListToPlan plan examSlots
    Nothing -> do
      hPutStrLn stderr $ "no planmanip file found: "
                        ++ file
                        ++ " does not exist or is not parsable."
      exitWith $ ExitFailure 6

applyAddRooms :: Config -> Plan -> IO Plan
applyAddRooms config plan =
  case roomsFile config of
    Just file -> do
      maybyRoomsForExams <- importAddRoomToExamFromYAMLFile file
      case maybyRoomsForExams of
        Just roomsForExams -> do
          hPutStrLn stderr ">>> adding rooms to exams"
          return $ applyAddRoomToExamListToPlan plan roomsForExams
        Nothing -> do
          hPutStrLn stderr $ "no rooms file found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 7
    Nothing -> do
      hPutStrLn stderr "no rooms file specified"
      return plan
