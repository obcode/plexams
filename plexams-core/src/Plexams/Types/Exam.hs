{-# LANGUAGE DeriveGeneric #-}

module Plexams.Types.Exam
  ( Exam(..)
  , registrations
  , isScheduled
  , isUnscheduled
  , withHandicaps
  , notPlannedByMe
  , seatsMissing
  ) where

import Data.Aeson
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (unpack)
import GHC.Generics

import Plexams.Types.Common
import Plexams.Types.Groups
import Plexams.Types.Persons
import Plexams.Types.Rooms

data Exam = Exam
  { anCode :: Ancode -- ^ Anmeldecode Prüfungsamt
  , name :: String -- ^ Name der Prüfung
  , lecturer :: Person -- ^ Prüfer
  , duration :: Duration -- ^ Dauer der Prüfung in Minuten
  , rooms :: [Room] -- ^ Liste der Räume in denen die Prüfung statt findet
  , plannedByMe :: Bool -- ^ @False@ bei Prüfungen, die zwar mit erfasst werden, aber nicht geplant werden
                             --   können
  , reExam :: Bool -- ^ @True@ bei einer Wiederholungsklausur
  , groups :: [Group] -- ^ Studierendengruppen die an der Prüfung teilnehmen
  , examType :: ExamType -- ^ Typ der Prüfung aus ZPA
  , slot :: Maybe (Int, Int) -- ^ (Tag, Slot)
  , registeredStudents :: [StudentWithRegs]
  , registeredStudentsCount :: Integer
  , registeredGroups :: [RegisteredGroup]
  , conflictingAncodes :: M.Map Ancode Integer -- ^ Ancode maps to number of students with this conflict
  , handicapStudents :: [StudentWithRegs]
  , sameRoom :: [Ancode]
  , sameSlot :: [Ancode]
  } deriving (Eq, Generic)

instance FromJSON Exam

instance ToJSON Exam

isScheduled :: Exam -> Bool
isScheduled = isJust . slot

isUnscheduled :: Exam -> Bool
isUnscheduled = not . isScheduled

withHandicaps :: Exam -> Bool
withHandicaps = not . null . handicapStudents

registrations :: Exam -> Integer
registrations -- sum . mapMaybe groupRegistrations . groups
                -- fromIntegral . length . registeredStudents
 = sum . map registeredGroupStudents . registeredGroups

notPlannedByMe :: [Ancode] -> Exam -> Exam
notPlannedByMe ancodes exam = exam {plannedByMe = anCode exam `notElem` ancodes}

instance Show Exam where
  show exam =
    show (anCode exam) ++
    ". " ++
    name exam ++
    ", " ++
    unpack (personShortName (lecturer exam)) ++
    (if reExam exam
       then ", W "
       else ", E ") ++
    show (registeredGroups exam) ++
    (if registrations exam > 0
       then "=" ++ show (registrations exam)
       else "") ++
    maybe "" ((", " ++) . show) (slot exam) ++
    (if null $ handicapStudents exam
       then ""
       else "\n        - " ++
            intercalate
              "\n        - "
              (map (show . studentName) (handicapStudents exam))) ++
    (if null $ rooms exam
       then ""
       else "\n        - " ++ intercalate "\n        - " (map show (rooms exam)))

seatsMissing :: Exam -> Integer
seatsMissing = toInteger . length . registeredStudents
  -- registrations exam - sum (map seatsPlanned $ rooms exam)
