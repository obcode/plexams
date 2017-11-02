module Plexams.Server.PlanManip
  ( appliedPlan
  , updateManipFile
  ) where

import           Control.Monad.Except
import qualified Data.ByteString          as BSI
import qualified Data.Yaml                as Y
import           Plexams.Import.PlanManip (importExamSlotsFromYAMLFile)
import           Plexams.PlanManip
import           Plexams.Server.Import
import           Plexams.Types

initialPlan' :: IO (Either String Plan)
initialPlan' = do
  semesterConfig'' <- liftIO (importSemesterConfig configFile')
  case semesterConfig'' of
    Left errorMsg -> return $ Left errorMsg
    Right config  -> initialPlan'' config

initialPlan'' :: SemesterConfig -> IO (Either String Plan)
initialPlan'' config = do
  exams'' <- liftIO $ importExams config
  case exams'' of
    Left errorMsg -> return $ Left errorMsg
    Right exams'  -> initialPlan''' config exams'

initialPlan''' :: SemesterConfig -> [Exam] -> IO (Either String Plan)
initialPlan''' config exams' = do
  persons'' <- liftIO $ importPersons config
  case persons'' of
    Left errorMsg  -> return $ Left errorMsg
    Right persons' -> initialPlan'''' config exams' persons'

initialPlan'''' :: SemesterConfig -> [Exam] -> Persons -> IO (Either String Plan)
initialPlan'''' config exams' persons' = do
  eitherStuds <- liftIO $ importStudents studentsFile'
  case eitherStuds of
    Left errorMsg -> return $ Left errorMsg
    Right students' -> do
      eitherHandicaps <- liftIO $ importHandicaps handicapsFile'
      case eitherHandicaps of
        Left errorMsg -> return $ Left errorMsg
        Right handicaps' -> return $ Right $ Plexams.PlanManip.makePlan exams' config persons' students' handicaps'

initPlanWCons :: IO (Either String Plan)
initPlanWCons = do
  constraints' <- importConstraints overlapsFile constraintsFile
  case constraints' of
    Left errorMsg -> return $ Left errorMsg
    Right constraints'' -> do
      plan' <- liftIO initialPlan'
      case plan' of
        Left errorMsg -> return $ Left errorMsg
        Right plan'' -> do
          return $ Right $ addConstraints constraints'' plan''


appliedPlan :: IO (Either String Plan)
appliedPlan = do
  plan' <- liftIO initPlanWCons
  case plan' of
    Left errorMsg -> return $ Left errorMsg
    Right plan'' -> do
      semesterConfig'' <- liftIO semesterConfig'
      case semesterConfig'' of
        Left errorMsg -> return $ Left errorMsg
        Right config' -> do
          appliedPlan' <- applyPlanManips' (planManipFile config') plan''
          return appliedPlan'

applyPlanManips' :: FilePath -> Plan -> IO (Either String Plan)
applyPlanManips' file plan = do
  maybeExamSlots <- importExamSlotsFromYAMLFile file
  case maybeExamSlots of
    Just examSlots'' -> do
      newPlan <- return $ applyAddExamToSlotListToPlan plan examSlots''
      return $ Right newPlan
    Nothing -> return $ Left errorMsg
      where
        errorMsg =   "no planmanip file found: "
                ++ file
                ++ " does not exist or is not parsable."

updateManipFile :: FilePath -> AddExamToSlot -> IO ()
updateManipFile outfile exam = do
  test1 <- importExamSlotsFromYAMLFile  outfile
  changed <- return $ changeSlot exam test1
  list <- return $ examSlotsToLists changed
  BSI.writeFile outfile $ Y.encode list

examSlotsToList :: AddExamToSlot -> [Integer]
examSlotsToList exam = [planManipAnCode exam
                        , toInteger $ planManipDay exam
                        , toInteger $ planManipSlot exam]

examSlotsToLists :: Maybe [AddExamToSlot] -> Maybe [[Integer]]
examSlotsToLists = fmap . map $ examSlotsToList

changeSlot :: AddExamToSlot -> Maybe [AddExamToSlot] -> Maybe [AddExamToSlot]
changeSlot exam exams' = fmap (++ [exam]) filtered
  where
    filtered = (fmap filterExam exams')
    filterExam exams'' = filter (\x -> (planManipAnCode x) /= (planManipAnCode exam)) exams''
