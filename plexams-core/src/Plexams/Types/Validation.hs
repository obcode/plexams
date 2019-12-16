{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.Validation
  ( ValidationResult(..)
  , ValidationRecord(..)
  , ValidationRecordType(..)
  , Validation(..)
  , ValidateWhat(..)
  , validateWhat
  , validationResult
  )
where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics

data Validation = Validation
  { result :: ValidationResult
  , brokenConstraints :: [ValidationRecord]
  } deriving (Generic)

instance ToJSON Validation

data ValidationResult
  = EverythingOk
  | SoftConstraintsBroken
  | HardConstraintsBroken
  deriving (Eq, Ord, Generic)

instance Show ValidationResult where
  show EverythingOk = "Validation ok!"
  show SoftConstraintsBroken = ">>> Soft Constraints broken <<<"
  show HardConstraintsBroken = ">>> Hard Constraints broken <<<"

instance ToJSON ValidationResult

-- data SoftConstraint = SoftConstraint Text
data ValidationRecord = ValidationRecord {
  validationRecordType :: ValidationRecordType,
  validationRecordText :: Text
 } deriving (Eq, Generic)

instance ToJSON ValidationRecord

data ValidationRecordType
  = Info
  | SoftConstraintBroken
  | HardConstraintBroken
  deriving (Eq, Generic)

instance ToJSON ValidationRecordType

validationResult :: [ValidationResult] -> ValidationResult
validationResult [] = EverythingOk
validationResult xs = maximum xs

data ValidateWhat
  = ValidateSources
  | ValidateSchedule
  | ValidateRooms
  | ValidateInvigilation
  deriving (Eq, Show, Generic, Enum)

validateWhat :: [ValidateWhat]
validateWhat = [ValidateSchedule .. ValidateInvigilation]

instance FromJSON ValidateWhat

instance ToJSON ValidateWhat
