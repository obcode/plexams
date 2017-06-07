{-# LANGUAGE OverloadedStrings #-}
module Plexams.Validation.Exports
  ( validateZPAExport
  ) where

import           Control.Monad.Writer
import           Data.Text            (Text, append)
import           Plexams.Import.Misc  (importZPAExamsFromJSONFile)
import           Plexams.Types
import           TextShow             (showt)

validateZPAExport :: FilePath -> Plan -> IO (ValidationResult, [Text])
validateZPAExport fp plan = do
  maybeZpaExams <- importZPAExamsFromJSONFile fp
  case maybeZpaExams of
    Nothing -> return ( HardConstraintsBroken
                      , ["ZPAExams cannot be imported from " `append` showt fp])
    Just zpaExams -> return $ runWriter $ validate zpaExams plan

validate :: [ZPAExam] -> Plan
         -> Writer [Text] ValidationResult
validate zpaExams plan = do
  tell ["# Validating ZPA export"]
  allOk <- allZPAExamsInExams zpaExams plan
  allExported <- allPlannedExamsInZPAExams zpaExams plan
  return $ validationResult [allOk, allExported]

allZPAExamsInExams :: [ZPAExam] -> Plan
                   -> Writer [Text] ValidationResult
allZPAExamsInExams zpaExams plan =
    validationResult <$> mapM (`zpaExamInExams` allExams plan) zpaExams
  where
    zpaExamInExams :: ZPAExam -> [Exam]
                   -> Writer [Text] ValidationResult
    zpaExamInExams zpaExam exams' =
      case filter ((== zpaExamAnCode zpaExam) . anCode) exams' of
        [exam] -> do
          plannedByMeOk <-
            if plannedByMe exam
            then return EverythingOk
            else do
              tell ["Exported exam " `append` showt (zpaExamAnCode zpaExam)
                    `append` " is not planned by me"]
              return HardConstraintsBroken
          dayOk <-
            if zpaExamDate zpaExam == examDateAsString exam plan
            then return EverythingOk
            else do
                tell ["Exported exam " `append` showt (zpaExamAnCode zpaExam)
                      `append` " has wrong date"]
                return HardConstraintsBroken
          slotOk <-
            if zpaExamTime zpaExam == examSlotAsString exam plan
            then return EverythingOk
            else do
                tell ["Exported exam " `append` showt (zpaExamAnCode zpaExam)
                      `append` " has wrong time"]
                return HardConstraintsBroken
          return $ validationResult [plannedByMeOk, dayOk, slotOk]
        _ -> do
          tell ["Exported exam " `append` showt (zpaExamAnCode zpaExam)
                `append` " does not exist"]
          return HardConstraintsBroken

allPlannedExamsInZPAExams :: [ZPAExam] -> Plan
                          -> Writer [Text] ValidationResult
allPlannedExamsInZPAExams zpaExams plan = do
    let examsPlannedByMe = filter plannedByMe $ allExams plan
    validationResult <$> mapM (`examExported` zpaExams) examsPlannedByMe
  where
    examExported :: Exam -> [ZPAExam] -> Writer [Text] ValidationResult
    examExported exam zpaExams' =
      if anCode exam `elem` map zpaExamAnCode zpaExams'
      then return EverythingOk
      else do
        tell ["Exam " `append` showt (anCode exam) `append` " not exported"]
        return HardConstraintsBroken
