{-# LANGUAGE OverloadedStrings #-}

module Plexams.Validation.Exports
  ( validateZPAExport
  ) where

import Control.Monad.Writer
import Data.Text (append)

import TextShow (showt)

import Plexams.Import (importZPAExamsFromJSONFile)
import Plexams.Types

validateZPAExport ::
     FilePath -> Plan -> IO (ValidationResult, [ValidationRecord])
validateZPAExport fp plan = do
  maybeZpaExams <- importZPAExamsFromJSONFile fp
  case maybeZpaExams of
    Nothing ->
      return
        ( HardConstraintsBroken
        , [ ValidationRecord HardConstraintBroken $
            "ZPAExams cannot be imported from " `append` showt fp
          ])
    Just zpaExams -> return $ runWriter $ validate zpaExams plan

validate :: [ZPAExam] -> Plan -> Writer [ValidationRecord] ValidationResult
validate zpaExams plan = do
  tell [ValidationRecord Info "# Validating ZPA export"]
  allOk <- allZPAExamsInExams zpaExams plan
  allExported <- allPlannedExamsInZPAExams zpaExams plan
  return $ validationResult [allOk, allExported]

allZPAExamsInExams ::
     [ZPAExam] -> Plan -> Writer [ValidationRecord] ValidationResult
allZPAExamsInExams zpaExams plan =
  validationResult <$> mapM (`zpaExamInExams` allExams plan) zpaExams
  where
    zpaExamInExams ::
         ZPAExam -> [Exam] -> Writer [ValidationRecord] ValidationResult
    zpaExamInExams zpaExam exams' =
      case filter ((== zpaExamAnCode zpaExam) . anCode) exams' of
        [exam] -> do
          plannedByMeOk <-
            if plannedByMe exam
              then return EverythingOk
              else do
                tell
                  [ ValidationRecord HardConstraintBroken $
                    "Exported exam " `append` showt (zpaExamAnCode zpaExam) `append`
                    " is not planned by me"
                  ]
                return HardConstraintsBroken
          dayOk <-
            if zpaExamDate zpaExam == examDateAsString exam plan
              then return EverythingOk
              else do
                tell
                  [ ValidationRecord HardConstraintBroken $
                    "Exported exam " `append` showt (zpaExamAnCode zpaExam) `append`
                    " has wrong date"
                  ]
                return HardConstraintsBroken
          slotOk <-
            if zpaExamTime zpaExam == examSlotAsString exam plan
              then return EverythingOk
              else do
                tell
                  [ ValidationRecord HardConstraintBroken $
                    "Exported exam " `append` showt (zpaExamAnCode zpaExam) `append`
                    " has wrong time"
                  ]
                return HardConstraintsBroken
          return $ validationResult [plannedByMeOk, dayOk, slotOk]
        _ -> do
          tell
            [ ValidationRecord HardConstraintBroken $
              "Exported exam " `append` showt (zpaExamAnCode zpaExam) `append`
              " does not exist"
            ]
          return HardConstraintsBroken

allPlannedExamsInZPAExams ::
     [ZPAExam] -> Plan -> Writer [ValidationRecord] ValidationResult
allPlannedExamsInZPAExams zpaExams plan = do
  let examsPlannedByMe = filter plannedByMe $ allExams plan
  validationResult <$> mapM (`examExported` zpaExams) examsPlannedByMe
  where
    examExported ::
         Exam -> [ZPAExam] -> Writer [ValidationRecord] ValidationResult
    examExported exam zpaExams' =
      if anCode exam `elem` map zpaExamAnCode zpaExams'
        then return EverythingOk
        else do
          tell
            [ ValidationRecord HardConstraintBroken $
              "Exam " `append` showt (anCode exam) `append` " not exported"
            ]
          return HardConstraintsBroken
