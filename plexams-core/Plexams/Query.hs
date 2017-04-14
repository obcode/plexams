module Plexams.Query
    ( allExams
    , scheduledExams
    , queryByAnCode
    , queryByGroup
    ) where

import qualified Data.Map       as M
import           Plexams.Import
import           Plexams.Types

allExams :: Plan -> [Exam]
allExams plan = let plan' = setSlotsOnExams plan
                in unscheduledExams plan' ++ scheduledExams plan'

scheduledExams :: Plan -> [Exam]
scheduledExams plan =
    concatMap (examsInSlot . snd) $ M.toList $ slots plan

queryByAnCode :: Integer -> Plan -> [Exam]
queryByAnCode ac = filter ((==ac) . anCode) . allExams

queryByGroup :: String -> Bool -> Plan -> [Exam]
queryByGroup group unscheduledOnly =
    filter (not . null . elemOrSubGroup (parseGroup group) . groups)
            . (if unscheduledOnly then unscheduledExams else allExams)
  where
    elemOrSubGroup :: Group -> [Group] -> [Group]
    elemOrSubGroup (Group degree Nothing Nothing _) groups =
        filter (\(Group d _ _ _) -> degree == d) groups
    elemOrSubGroup (Group degree semester Nothing _) groups =
        filter (\(Group d s _ _) -> degree == d && semester == s) groups
    elemOrSubGroup group groups = filter (==group) groups
