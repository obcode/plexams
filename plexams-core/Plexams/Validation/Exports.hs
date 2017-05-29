module Plexams.Validation.Exports
  ( validateZPAExport
  ) where

import           Control.Monad.Writer
import           Plexams.Import.Misc (importZPAExamsFromJSONFile)
import           Plexams.Query             (allExams)
import           Plexams.Types

validateZPAExport :: FilePath -> Plan -> IO (ValidationResult, [String])
validateZPAExport fp plan = do
  maybeZpaExams <- importZPAExamsFromJSONFile fp
  case maybeZpaExams of
    Nothing -> return ( HardConstraintsBroken
                      , ["ZPAExams cannot be imported from "++ fp])
    Just zpaExams -> return $ runWriter $ validate zpaExams plan

validate :: [ZPAExam] -> Plan
         -> Writer [String] ValidationResult
validate zpaExams plan = do
  tell ["# Validating ZPA export"]
  allOk <- allZPAExamsInExams zpaExams plan
  allExported <- allPlannedExamsInZPAExams zpaExams plan
  return $ validationResult [allOk, allExported]

allZPAExamsInExams :: [ZPAExam] -> Plan
                   -> Writer [String] ValidationResult
allZPAExamsInExams zpaExams plan =
    validationResult <$> mapM (`zpaExamInExams` allExams plan) zpaExams
  where
    zpaExamInExams :: ZPAExam -> [Exam]
                   -> Writer [String] ValidationResult
    zpaExamInExams zpaExam exams =
      case filter ((== zpaExamAnCode zpaExam) . anCode) exams of
        [] -> do
          tell ["Exported exam " ++ show (zpaExamAnCode zpaExam)
                ++ " does not exist"]
          return HardConstraintsBroken
        [exam] -> do
          plannedByMeOk <-
            if plannedByMe exam
            then return EverythingOk
            else do
              tell ["Exported exam " ++ show (zpaExamAnCode zpaExam)
                    ++ " is not planned by me"]
              return HardConstraintsBroken
          dayOk <-
            if zpaExamDate zpaExam == examDateAsString exam plan
            then return EverythingOk
            else do
                tell ["Exported exam " ++ show (zpaExamAnCode zpaExam)
                      ++ " has wrong date"]
                return HardConstraintsBroken
          slotOk <-
            if zpaExamTime zpaExam == examSlotAsString exam plan
            then return EverythingOk
            else do
                tell ["Exported exam " ++ show (zpaExamAnCode zpaExam)
                      ++ " has wrong time"]
                return HardConstraintsBroken
          return $ validationResult [plannedByMeOk, dayOk, slotOk]

allPlannedExamsInZPAExams :: [ZPAExam] -> Plan
                          -> Writer [String] ValidationResult
allPlannedExamsInZPAExams zpaExams plan = do
    let examsPlannedByMe = filter plannedByMe $ allExams plan
    validationResult <$> mapM (`examExported` zpaExams) examsPlannedByMe
  where
    examExported :: Exam -> [ZPAExam] -> Writer [String] ValidationResult
    examExported exam zpaExams =
      if anCode exam `elem` map zpaExamAnCode zpaExams
      then return EverythingOk
      else do
        tell ["Exam " ++ show (anCode exam) ++ " not exported"]
        return HardConstraintsBroken
