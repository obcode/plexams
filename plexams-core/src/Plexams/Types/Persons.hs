{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Plexams.Types.Persons
  ( Person(..)
  , Persons
  , Students
  , MtkNr
  , StudentName
  , StudentsExams
  , StudentsWithRegs
  , StudentWithRegs(..)
  , Handicap(..)
  , Invigilator(..)
  , Invigilators
  , addHandicaps
  ) where

import           Control.Applicative  (empty)
import           Data.Aeson
import           Data.List            ((\\))
import qualified Data.Map             as M
import           Data.Monoid          ((<>))
import qualified Data.Set             as S
import           Data.Text            (Text, unpack)
import qualified Data.Yaml            as Y
import           GHC.Generics
import           Plexams.Types.Common
import           TextShow             (TextShow, showb)

type Persons = M.Map PersonID Person

data Person = Person
    { personID        :: PersonID
    , personShortName :: Text
    , personFullName  :: Text
    , personEmail     :: Text
    , personFK        :: Text
    , personIsLBA     :: Bool
    }
  deriving (Eq, Show, Ord, Generic)

instance TextShow Person where
  showb (Person iD shortName _ email _ _) =
    showb iD <> ". " <> showb shortName
    <> " <" <> showb email <> "> "

instance FromJSON Person where
    parseJSON (Object v) = Person
                        <$> v .: "person_id"
                        <*> v .: "person_shortname"
                        <*> v .: "person_fullname"
                        <*> v .: "email"
                        <*> v .: "fk"
                        <*> v .: "is_lba"
    parseJSON _          = empty

instance ToJSON Person

type MtkNr = Integer
type StudentName = Text
type Students = M.Map Ancode (S.Set (MtkNr, StudentName))
type StudentsExams = M.Map MtkNr (StudentName, S.Set Ancode)

type StudentsWithRegs = M.Map MtkNr StudentWithRegs

data StudentWithRegs = StudentWithRegs
  { studentMtknr    :: Integer
  , studentName     :: Text
  , studentGroup    :: Text
  , studentAncodes  :: [Ancode]
  , studentHandicap :: Maybe Handicap
  }
  deriving (Eq, Generic)

instance FromJSON StudentWithRegs
instance ToJSON StudentWithRegs

data Handicap = Handicap
  { handicapStudentname          :: Text
  , handicapMtknr                :: Integer
  , handicapCompensationText     :: Text
  , handicapDeltaDurationPercent :: Integer
  , handicapNotForExams          :: [Ancode]
  , handicapNeedsRoomAlone       :: Bool
  }
  deriving (Eq,Generic)

instance Y.FromJSON Handicap where
  parseJSON (Y.Object v) = Handicap
                        <$> v Y..: "studentname"
                        <*> v Y..: "mtknr"
                        <*> v Y..: "compensation"
                        <*> v Y..: "deltaDurationPercent"
                        <*> v Y..:? "notForExams" Y..!= []
                        <*> v Y..:? "needsRoomAlone" Y..!= False
  parseJSON _            = empty

instance ToJSON Handicap

addHandicaps :: StudentsWithRegs -> [Handicap] -> StudentsWithRegs
addHandicaps = foldr addHandicap
  where
    addHandicap :: Handicap -> StudentsWithRegs -> StudentsWithRegs
    addHandicap handicap = M.alter (maybe Nothing
      (\s -> Just $ s { studentHandicap = Just handicap
                      , studentAncodes = studentAncodes s
                                          \\ handicapNotForExams handicap
                      })) (handicapMtknr handicap)

instance Show Handicap where
  show handicap = unpack (handicapStudentname handicap)
                  ++ " (" ++ unpack (handicapCompensationText handicap) ++ ")"

type InvigilatorID = Integer

data Invigilator = Invigilator
  { invigilatorExcludedDays         :: [Int]
  , invigilatorExamDays             :: [Int]
  , invigilatorWantDays             :: [Int]
  , invigilatorCanDays              :: [Int]
  , invigilatorPerson               :: Maybe Person
  , invigilatorMinutesTodo          :: Integer
  , invigilatorsMinutesPlanned      :: Integer
  , invigilatorName                 :: Text
  , invigilatorID                   :: Integer
  , invigilatorExcludedDates        :: [Text]
  , invigilatorPartTime             :: Float
  , invigilatorFreeSemester         :: Float
  , invigilatorOvertimeThisSemester :: Float
  , invigilatorOvertimeLastSemester :: Float
  , invigilatorOralExams            :: Integer
  , invigilatorMaster               :: Integer
  , invigilatorLiveCoding           :: Integer
  } deriving (Show, Eq, Generic)

instance FromJSON Invigilator where
    parseJSON (Object v ) = Invigilator [] [] [] [] Nothing 0 0
                         <$> v .: "invigilator"
                         <*> v .: "inviligator_id"
                         <*> v .: "excluded_dates"
                         <*> v .: "part_time"
                         <*> v .: "free_semester"
                         <*> v .: "overtime_this_semester"
                         <*> v .: "overtime_last_semester"
                         <*> v .: "oral_exams_contribution"
                         <*> v .: "master_contribution"
                         <*> v .: "livecoding_contribution"
    parseJSON _          = empty

instance ToJSON Invigilator

type Invigilators = M.Map InvigilatorID Invigilator
