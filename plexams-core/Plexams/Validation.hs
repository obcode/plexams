module Plexams.Validation
    ( validatePlan
    ) where

import           Control.Monad.Writer
import           Plexams.Types

validatePlan :: Plan -> (Bool, [String])
validatePlan = runWriter . validatePlan'

validatePlan' :: Plan -> Writer [String] Bool
validatePlan' plan = do
    tell ["no validations implemented yet"]
    return False
