{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Plexams.Types.Persons
  ( Person(..)
  , Persons
  , Students
  , MtkNr
  , StudentName
  , StudentsExams
  , Handicap(..)
  , Invigilator(..)
  , Invigilators
  ) where

import           Control.Applicative  (empty)
import           Data.Aeson           (FromJSON, Value (Object), parseJSON,
                                       (.:))
import           Data.Aeson
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

data Handicap = Handicap
  { studentname          :: Text
  , mtknr                :: Integer
  , compensation         :: Text
  , deltaDurationPercent :: Integer
  , exams                :: [Ancode]
  , needsRoomAlone       :: Bool
  }
  deriving (Eq,Generic)

instance Y.FromJSON Handicap where
  parseJSON (Y.Object v) = Handicap
                        <$> v Y..: "studentname"
                        <*> v Y..: "mtknr"
                        <*> v Y..: "compensation"
                        <*> v Y..: "deltaDurationPercent"
                        <*> v Y..: "exams"
                        <*> v Y..:? "needsRoomAlone" Y..!= False
  parseJSON _            = empty

instance ToJSON Handicap

instance Show Handicap where
  show handicap = unpack (studentname handicap)
                  ++ " (" ++ unpack (compensation handicap) ++ ")"

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
    parseJSON _          = empty

instance ToJSON Invigilator

type Invigilators = M.Map InvigilatorID Invigilator
