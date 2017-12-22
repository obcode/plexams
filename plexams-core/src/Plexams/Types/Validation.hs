{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.Validation
  ( ValidationResult(..)
  , ValidationRecord(..)
  , Validation(..)
  , ValidateWhat(..)
  , validateWhat
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
                      | ValidationListStart Text
                      | ValidationListEnd
  deriving (Eq, Generic)

instance ToJSON ValidationRecord

validationMessage :: ValidationRecord -> Text
validationMessage (Info msg) = msg
validationMessage (SoftConstraintBroken msg) = msg
validationMessage (HardConstraintBroken msg) = msg
validationMessage (ValidationListStart msg) = msg
validationMessage ValidationListEnd = ""

validationResult :: [ValidationResult] -> ValidationResult
validationResult = maximum

data ValidateWhat = ValidateSources
                  | ValidateSchedule
                  | ValidateRooms
                  | ValidateInvigilation
  deriving (Eq, Show, Generic, Enum)

validateWhat :: [ValidateWhat]
validateWhat = [ValidateSchedule .. ValidateInvigilation]

instance FromJSON ValidateWhat
instance ToJSON ValidateWhat
