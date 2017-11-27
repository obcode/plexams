{-# LANGUAGE DeriveGeneric #-}

module Plexams.Types.Validation
  ( ValidationResult(..)
  , ValidationRecord(..)
  , Validation(..)
  , ValidateWhat(..)
  , validationMessage
  , validationResult
  ) where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

data Validation = Validation
  { result            :: ValidationResult
  , brokenConstraints :: [ValidationRecord]
  }
  deriving (Generic)

instance ToJSON Validation

data ValidationResult = EverythingOk
                      | SoftConstraintsBroken
                      | HardConstraintsBroken
  deriving (Eq, Ord, Generic)

instance Show ValidationResult where
  show EverythingOk          = "Validation ok!"
  show SoftConstraintsBroken = ">>> Soft Constraints broken <<<"
  show HardConstraintsBroken = ">>> Hard Constraints broken <<<"

instance ToJSON ValidationResult

-- data SoftConstraint = SoftConstraint Text

data ValidationRecord = Info Text
                      | SoftConstraintBroken Text
                      | HardConstraintBroken Text
  deriving (Eq, Generic)

instance ToJSON ValidationRecord

validationMessage :: ValidationRecord -> Text
validationMessage (Info msg) = msg
validationMessage (SoftConstraintBroken msg) = msg
validationMessage (HardConstraintBroken msg) = msg

validationResult :: [ValidationResult] -> ValidationResult
validationResult = maximum

data ValidateWhat = ValidateSources
                  | ValidateSchedule
                  | ValidateRooms
                  | ValidateInvigilation
  deriving (Eq, Show)
