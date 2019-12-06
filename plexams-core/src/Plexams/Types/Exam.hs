{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.Exam
  ( Exam(..)
  , registrations
  , isScheduled
  , isUnscheduled
  , isGOExam
  , withHandicaps
  , handicapStudentsNeedsRoomAlone
  , withHandicapsNeedsRoomAlone
  , notPlannedByMe
  , seatsMissing
  , showMinimal
  )
where

import           Data.Aeson
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           GHC.Generics

import           Plexams.Types.Common
import           Plexams.Types.Groups
import           Plexams.Types.Persons
import           Plexams.Types.Rooms

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
  , shareRoom :: Bool
  , unregisteredStudents :: Integer -- TODO: für Raumplanung zusätzliche Studierende anderer Fakultäten, die ich nicht in Sammellisten habe
  -- Besser: Fakestudenten mit Fakeidentität hinzufügen...
  , onOtherCampus :: Bool -- TODO: for conflicts more slots, e.g., FK10
  , stakeholder :: [Text] -- TODO: inform about exam, e.g., FK13, ...
  } deriving (Eq, Generic)

instance FromJSON Exam

instance ToJSON Exam

isGOExam :: Exam -> Bool
isGOExam exam = if not $ null $ registeredGroups exam
  then
    let g = map registeredGroupDegree (registeredGroups exam)
    in  "GO" `elem` g || "GN" `elem` g
  else GO `elem` map groupDegree (groups exam)

isScheduled :: Exam -> Bool
isScheduled = isJust . slot

isUnscheduled :: Exam -> Bool
isUnscheduled = not . isScheduled

withHandicaps :: Exam -> Bool
withHandicaps = not . null . handicapStudents

handicapStudentsNeedsRoomAlone :: Exam -> [StudentWithRegs]
handicapStudentsNeedsRoomAlone =
  filter (maybe False handicapNeedsRoomAlone . studentHandicap)
    . handicapStudents

withHandicapsNeedsRoomAlone :: Exam -> Bool
withHandicapsNeedsRoomAlone = not . null . handicapStudentsNeedsRoomAlone

registrations :: Exam -> Integer
registrations = sum . map registeredGroupStudents . registeredGroups -- sum . mapMaybe groupRegistrations . groups
                -- fromIntegral . length . registeredStudents

notPlannedByMe :: [Ancode] -> Exam -> Exam
notPlannedByMe ancodes exam =
  exam { plannedByMe = anCode exam `notElem` ancodes }

instance Show Exam where
  show exam =
    showMinimal exam ++
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

showMinimal :: Exam -> String
showMinimal exam = show (anCode exam) ++ ". " ++ name exam ++ ", " ++ unpack
  (personShortName (lecturer exam))

seatsMissing :: Exam -> Integer
seatsMissing = toInteger . length . registeredStudents
  -- registrations exam - sum (map seatsPlanned $ rooms exam)
