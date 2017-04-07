module Plexams.Query
    ( allExams
    , scheduledExams
    , queryByAnCode
    , queryByGroup
    ) where

import           Plexams.Import
import           Plexams.Types
import qualified Data.Map as M

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
    elemOrSubGroup (Group degree Nothing Nothing) groups =
        filter (\(Group d _ _) -> degree == d) groups
    elemOrSubGroup (Group degree semester Nothing) groups =
        filter (\(Group d s _) -> degree == d && semester == s) groups
    elemOrSubGroup group groups = filter (==group) groups
