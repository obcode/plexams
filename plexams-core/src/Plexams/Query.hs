module Plexams.Query
    ( queryByAnCode
    , queryByName
    , queryByLecturer
    , queryByGroup
    , querySlot
    , examDaysPerLecturer
    , lecturerExamDays
    , examsWithSameName
    , queryStudentByName
    ) where

import           Data.List     (isInfixOf, nub, sortBy)
import qualified Data.Map      as M
import           Data.Maybe    (maybe)
import           Data.Set      (Set)
import           Data.Text     (unpack)
import           GHC.Exts      (groupWith)
import           Plexams.Types

queryByAnCode :: Integer -> Plan -> [Exam]
queryByAnCode ac = filter ((==ac) . anCode) . allExams

queryByName :: String -> Plan -> [Exam]
queryByName str = filter (isInfixOf str . name) . allExams

queryByLecturer :: String -> Plan -> [Exam]
queryByLecturer str =
  filter (isInfixOf str . unpack . personShortName . lecturer) . allExams

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
    elemOrSubGroup (Group degree Nothing Nothing _) groups' =
        filter (\(Group d _ _ _) -> degree == d) groups'
    elemOrSubGroup (Group degree semester' Nothing _) groups' =
        filter (\(Group d s _ _) -> degree == d && semester' == s) groups'
    elemOrSubGroup group' groups' = filter (==group') groups'

querySlot :: (Int, Int) -> Plan -> [Exam]
querySlot s = maybe [] (M.elems . examsInSlot)
            . M.lookup s
            . slots

examDaysPerLecturer :: Plan -> M.Map PersonID [DayIndex]
examDaysPerLecturer = M.fromList
      . map (\g -> (fst $ head g, nub $ map snd g))
      . groupWith fst
      . concatMap (\((d,_), s) -> map (\e -> (personID $ lecturer e, d))
                                      $ M.elems
                                      $ examsInSlot s)
      . M.toList
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

queryStudentByName :: String -> Plan -> [(MtkNr, (StudentName, Set Ancode))]
queryStudentByName str =
  filter (isInfixOf str . unpack .  fst . snd)
  . M.toList
  . studentsExams
