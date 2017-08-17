{-# LANGUAGE DeriveGeneric #-}

module Plexams.Server.PlanManip where

import           Control.Monad.Except
import           Data.Aeson
import           GHC.Generics
import           Plexams.Import.PlanManip (importExamSlotsFromYAMLFile)
import           Plexams.PlanManip
import           Plexams.Server.Import
import           Plexams.Types


data AddExam = AddExam {
  anCode1 :: Ancode,
  day     :: DayIndex,
  slot1   :: SlotIndex
} deriving (Eq, Show, Generic)

instance ToJSON AddExam
instance FromJSON AddExam

initialPlan' :: IO (Either String Plan)
initialPlan' = do
  semesterConfig'' <- liftIO (importSemesterConfig configFile')
  case semesterConfig'' of
    Left errorMsg -> return (Left errorMsg)
    Right config  -> (initialPlan'' config)

initialPlan'' :: SemesterConfig -> IO (Either String Plan)
initialPlan'' config = do
  exams'' <- liftIO (importExams config)
  case exams'' of
    Left errorMsg -> return (Left errorMsg)
    Right exams'  -> (initialPlan''' config exams')

initialPlan''' :: SemesterConfig -> [Exam] -> IO (Either String Plan)
initialPlan''' config exams' = do
  persons'' <- liftIO (importPersons config)
  case persons'' of
    Left errorMsg  -> return (Left errorMsg)
    Right persons' -> (initialPlan'''' config exams' persons')

initialPlan'''' :: SemesterConfig -> [Exam] -> Persons -> IO (Either String Plan)
initialPlan'''' config exams' persons' = do
  students' <- liftIO (importStudents (studentsFile'))
  handicaps' <- liftIO (importHandicaps (Just handicapsFile'))
  return (Right (Plexams.PlanManip.makePlan exams' config persons' students' handicaps'))

appliedPlan :: IO (Either String Plan)
appliedPlan = do
  plan' <- liftIO initialPlan'
  case plan' of
    Left error' -> return $ Left error'
    Right plan'' -> do
      semesterConfig'' <- liftIO semesterConfig'
      case semesterConfig'' of
        Left error'' -> return $ Left error''
        Right config' -> do
          appliedPlan' <- applyPlanManips' (planManipFile config') plan''
          return appliedPlan'

applyPlanManips' :: FilePath -> Plan -> IO (Either String Plan)
applyPlanManips' file plan = do
  maybeExamSlots <- importExamSlotsFromYAMLFile file
  case maybeExamSlots of
    Just examSlots'' -> do
      newPlan <- return (applyAddExamToSlotListToPlan plan examSlots'')
      return (Right newPlan)
    Nothing -> return $ Left error'
      where
        error' =   "no planmanip file found: "
                ++ file
                ++ " does not exist or is not parsable."

appendManipFile :: FilePath -> AddExam -> IO ()
appendManipFile outfile exam =
  Prelude.appendFile outfile $ "- [" ++ show (anCode1 exam) ++ ", " ++ show (day exam) ++ ", " ++ show (slot1 exam) ++ "] \n"
