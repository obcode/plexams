{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.ZPA
  ( ZPAExam(..)
  , ZPARoom(..)
  , StudentsExam(..)
  ) where

import Control.Applicative (empty)
import Data.Aeson
       (FromJSON, ToJSON, Value(Object), (.:), (.=), object, parseJSON,
        toJSON)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics

data ZPAExam = ZPAExam
  { zpaExamAnCode :: Integer
  , zpaExamDate :: String
  , zpaExamTime :: String
  , zpaTotalNumber :: Integer
  , zpaExamReserveInvigilatorId :: Integer
  , zpaExamRooms :: [ZPARoom]
  } deriving (Generic)

instance FromJSON ZPAExam where
  parseJSON (Object v) =
    ZPAExam <$> v .: "anCode" <*> v .: "date" <*> v .: "time" <*>
    v .: "total_number" <*>
    v .: "reserveInvigilator_id" <*>
    v .: "rooms"
  parseJSON _ = empty

instance ToJSON ZPAExam where
  toJSON (ZPAExam anCode' date time totalNumber reserveInvigilator' rooms') =
    object
      [ "anCode" .= anCode'
      , "date" .= date
      , "time" .= time
      , "total_number" .= totalNumber
      , "reserveInvigilator_id" .= reserveInvigilator'
      , "rooms" .= V.fromList (map toJSON rooms')
      ]

data ZPARoom = ZPARoom
  { zpaRoomNumber :: String
  , zpaRoomInvigilatorId :: Integer
  , zpaRoomReserveRoom :: Bool
  , zpaRoomHandicapCompensation :: Bool
  , zpaRoomDuration :: Integer
  , zpaRoomNumberStudents :: Integer
  } deriving (Generic)

instance FromJSON ZPARoom where
  parseJSON (Object v) =
    ZPARoom <$> v .: "number" <*> v .: "invigilator_id" <*> v .: "reserveRoom" <*>
    v .: "handicapCompensation" <*>
    v .: "duration" <*>
    v .: "numberStudents"
  parseJSON _ = empty

instance ToJSON ZPARoom where
  toJSON (ZPARoom number invigilator' reserve handicap duration' numberStudents) =
    object
      [ "number" .= number
      , "invigilator_id" .= invigilator'
      , "reserveRoom" .= reserve
      , "handicapCompensation" .= handicap
      , "duration" .= duration'
      , "numberStudents" .= numberStudents
      ]

data StudentsExam = StudentsExam
  { studentsExamAnCode :: Integer
  , studentsExamName :: String
  , studentsExamLecturerName :: Text
  , studentsExamDate :: String
  , studentsExamTime :: String
  } deriving (Generic)

instance FromJSON StudentsExam where
  parseJSON (Object v) =
    StudentsExam <$> v .: "anCode" <*> v .: "name" <*> v .: "lecturerName" <*>
    v .: "date" <*>
    v .: "time"
  parseJSON _ = empty

instance ToJSON StudentsExam where
  toJSON (StudentsExam anCode' name lecturerName date time) =
    object
      [ "anCode" .= anCode'
      , "name" .= name
      , "lecturerName" .= lecturerName
      , "date" .= date
      , "time" .= time
      ]
