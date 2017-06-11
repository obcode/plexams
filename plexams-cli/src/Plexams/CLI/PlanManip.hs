module Plexams.CLI.PlanManip
  ( makePlan
  , applyPlanManips
  , applyAddRooms
  , addInvigilatorsToPlan
  , applyAddInvigilators
  ) where

import           Control.Monad            (when)
import           Plexams.CLI.Import
import           Plexams.CLI.Types
import           Plexams.Import.PlanManip
import           Plexams.Invigilation
import           Plexams.PlanManip        hiding (makePlan)
import qualified Plexams.PlanManip
import           Plexams.Types
import           System.Directory         (doesFileExist)
import           System.Exit
import           System.IO                (hPutStrLn, stderr)
import           Text.Show.Pretty         (ppShow)

makePlan :: Config -> IO Plan
makePlan config = do
    semesterConfig' <- importSemesterConfig config
    exams'          <- importExams semesterConfig'
    persons'        <- importPersons semesterConfig'
    examsWithRegs   <- importAndAddRegs config exams'
    maybeStudents   <- importStudents config
    constraints'    <- importConstraints config
    handicaps'      <- importHandicaps config
    let plan = setHandicapsOnScheduledExams
           $ addConstraints constraints'
           $ Plexams.PlanManip.makePlan examsWithRegs
                                        semesterConfig'
                                        persons'
                                        maybeStudents
                                        handicaps'
    when (verbose config) $ do
      hPutStrLn stderr $ ppShow semesterConfig'
      hPutStrLn stderr $ ppShow exams'
      hPutStrLn stderr $ ppShow persons'
      hPutStrLn stderr $ ppShow examsWithRegs
      hPutStrLn stderr $ ppShow maybeStudents
      hPutStrLn stderr $ ppShow constraints'
      hPutStrLn stderr $ ppShow handicaps'
      hPutStrLn stderr $ ppShow plan
    return plan

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
      maybeRoomsForExams <- importAddRoomToExamFromYAMLFile file
      case maybeRoomsForExams of
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

addInvigilatorsToPlan :: Config -> Plan -> IO Plan
addInvigilatorsToPlan config plan = do
  invigilators'   <- importInvigilators config
  let plan' = addInvigilators invigilators' plan
  when (verbose config) $
    hPutStrLn stderr $ ppShow invigilators'
  return plan'

applyAddInvigilators :: Config -> Plan -> IO Plan
applyAddInvigilators config plan =
  case addInvigilatorFile config of
    Just file -> do
      maybeInvigilators <- importAddInvigilatorToRoomOrSlotFromYAMLFile file
      case maybeInvigilators of
        Just invigilatorsForSlots -> do
          hPutStrLn stderr ">>> adding invigilators to exams and slots"
          return $ invigilatorAddMinutes
                 $ applyAddInvigilatorsToPlan plan invigilatorsForSlots
        Nothing -> do
          hPutStrLn stderr $ "no add-invigilators file found: "
                            ++ file
                            ++ " does not exist or is not parsable."
          exitWith $ ExitFailure 17
    Nothing -> do
      hPutStrLn stderr "no add-invigilators file specified"
      return $ invigilatorAddMinutes plan
