module Plexams.Types.Validation
  ( ValidationResult(..)
  , validationResult
  ) where

data ValidationResult = EverythingOk
                      | SoftConstraintsBroken
                      | HardConstraintsBroken
  deriving (Eq, Ord)

instance Show ValidationResult where
  show EverythingOk          = "Validation ok!"
  show SoftConstraintsBroken = ">>> Soft Constraints broken <<<"
  show HardConstraintsBroken = ">>> Hard Constraints broken <<<"

validationResult :: [ValidationResult] -> ValidationResult
validationResult = maximum
