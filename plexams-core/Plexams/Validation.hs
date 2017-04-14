module Plexams.Validation
    ( validatePlan
    ) where

import           Control.Monad.Writer
import qualified Data.Map             as M
import           Data.Maybe           (mapMaybe)
import           Plexams.Types

validatePlan :: Plan -> (Bool, [String])
validatePlan = runWriter . validatePlan'

validatePlan' :: Plan -> Writer [String] Bool
validatePlan' plan = do
  goSlotsOk <- validateGOSlots plan
  tell ["# no more validations implemented yet"]
  return $ goSlotsOk && False

validateGOSlots :: Plan -> Writer [String] Bool
validateGOSlots plan = do
  tell ["# Checking GO-Slots!"]
  let allowedSlots = goSlots $ semesterConfig plan
      examsInOtherSlots = filter (elem GO . map groupDegree . groups)
                        $ concatMap examsInSlot
                        $ mapMaybe (`M.lookup` slots plan)
                        $ filter (not . (`elem` allowedSlots))
                        $ M.keys $ slots plan
      goOk = null examsInOtherSlots
  unless goOk $
    forM_ examsInOtherSlots $ \exam ->
      tell [ "- GO exam " ++ show (anCode exam) ++ " in wrong slot" ]
  return goOk
