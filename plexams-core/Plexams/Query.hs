module Plexams.Query
    ( allExams
    , scheduledExams
    , queryByAnCode
    , queryByName
    , queryByLecturer
    , queryByGroup
    , querySlot
    , lecturerExamDays
    , examsWithSameName
    ) where

import           Data.List      (isInfixOf, sortBy)
import qualified Data.Map       as M
import           Data.Maybe     (maybe)
import           GHC.Exts       (groupWith)
import           Plexams.Import
import           Plexams.Types

allExams :: Plan -> [Exam]
allExams plan = let plan' = setSlotsOnExams plan
                in M.elems (unscheduledExams plan') ++ scheduledExams plan'

scheduledExams :: Plan -> [Exam]
scheduledExams plan =
    concatMap (M.elems . examsInSlot . snd) $ M.toList $ slots plan

queryByAnCode :: Integer -> Plan -> [Exam]
queryByAnCode ac = filter ((==ac) . anCode) . allExams

queryByName :: String -> Plan -> [Exam]
queryByName str = filter (isInfixOf str . name) . allExams

queryByLecturer :: String -> Plan -> [Exam]
queryByLecturer str =
  filter (isInfixOf str . personShortName . lecturer) . allExams

queryByGroup :: String -> Bool -> Plan -> [Exam]
queryByGroup group unscheduledOnly =
    filter (not . null . elemOrSubGroup (parseGroup group) . groups)
            . (if unscheduledOnly
                  then sortBy (\e1 e2 -> compare (registrations e2)
                                                 (registrations e1)
                              )
                       . M.elems . unscheduledExams
                  else allExams)
  where
    elemOrSubGroup :: Group -> [Group] -> [Group]
    elemOrSubGroup (Group degree Nothing Nothing _) groups =
        filter (\(Group d _ _ _) -> degree == d) groups
    elemOrSubGroup (Group degree semester Nothing _) groups =
        filter (\(Group d s _ _) -> degree == d && semester == s) groups
    elemOrSubGroup group groups = filter (==group) groups

querySlot :: (Int, Int) -> Plan -> [Exam]
querySlot s = maybe [] (M.elems . examsInSlot)
            . M.lookup s
            . slots

lecturerExamDays :: Plan -> [(Person, [Int])]
lecturerExamDays =
  map (\list -> (fst (head list), map snd list))
  . groupWith fst
  . concatMap (\((d,_), lecturers) -> map (\l -> (l, d)) lecturers )
  . M.toList
  . M.map (map lecturer . M.elems . examsInSlot)
  . slots

examsWithSameName :: Plan -> [[Exam]]
examsWithSameName =  filter ((>1) . length)
                  . groupWith name
                  . allExams
