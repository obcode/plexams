{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.ZPA
  ( planToZPA
  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Time                  (Day)
import           Data.Time.Calendar
import qualified Data.Vector                as V
import           GHC.Exts                   (sortWith)
import           GHC.Generics
import           Plexams.Types

-- TODO: Anreichern mit Infos über die FK10-Prüfungen

--------------------------------------------------------------------------------
-- Export for ZPA
--------------------------------------------------------------------------------
dateString :: Day -> String
dateString day = let (y, m, d) = toGregorian day
                     showWith0 n = let s = show n
                                   in if n < 10 then "0"++s else s
                 in showWith0 d ++ "." ++ showWith0 m ++ "." ++ show y

data ZPAExam = ZPAExam
    { zpaExamAnCode               :: Integer
    , zpaExamDate                 :: String
    , zpaExamTime                 :: String
    , zpaExamReserveInvigilatorId :: Integer
    , zpaExamRooms                :: [ZPARoom]
    }
  deriving (Generic)

instance ToJSON ZPAExam where
  toJSON (ZPAExam anCode date time reserveInvigilator rooms) =
    object [ "anCode" .= anCode
           , "date"   .= date
           , "time"   .= time
           , "reserveInvigilator_id" .= reserveInvigilator
           , "rooms"  .= V.fromList (map toJSON rooms)
           ]

-- TODO: Zeit der FK10-Examen für den Export setzen!
examToZPAExam :: SemesterConfig -> Maybe Integer -> Exam -> ZPAExam
examToZPAExam semesterConfig reserveInvigilator exam = ZPAExam
    { zpaExamAnCode = anCode exam
    , zpaExamDate = date
    , zpaExamTime = time
    , zpaExamReserveInvigilatorId = fromMaybe 0 reserveInvigilator
    , zpaExamRooms = map (roomToZPARoom $ duration exam) $ rooms exam
    }
  where
    slotOfExam = slot exam
    days = examDays semesterConfig
    date = maybe "" (dateString . (days!!) . fst) slotOfExam
    times = slotsPerDay semesterConfig
    time = maybe "" ((times!!) . snd) slotOfExam

data ZPARoom = ZPARoom
    { zpaRoomNumber               :: String
    , zpaRoomInvigilatorId        :: Integer
    , zpaRoomReserveRoom          :: Bool
    , zpaRoomHandicapCompensation :: Bool
    , zpaRoomDuration             :: Integer
    }
  deriving (Generic)

instance ToJSON ZPARoom where
    toJSON (ZPARoom number invigilator reserve handicap duration) =
      object [ "number"               .= number
             , "invigilator_id"       .= invigilator
             , "reserveRoom"          .= reserve
             , "handicapCompensation" .= handicap
             , "duration"             .= duration
             ]

roomToZPARoom :: Integer -> Room -> ZPARoom
roomToZPARoom duration room = ZPARoom
    { zpaRoomNumber = roomID room
    , zpaRoomInvigilatorId = fromMaybe 0 $ invigilator room
    , zpaRoomReserveRoom = reserveRoom room
    , zpaRoomHandicapCompensation = handicapCompensation room
    , zpaRoomDuration = duration + deltaDuration room
    }

planToZPAExams :: Plan -> [ZPAExam]
planToZPAExams plan = map (uncurry (examToZPAExam (semesterConfig plan)))
               $ scheduledExamsWithReserveInvigilator plan

scheduledExamsWithReserveInvigilator :: Plan -> [(Maybe Integer, Exam)]
scheduledExamsWithReserveInvigilator =
    sortWith (anCode . snd)
    . filter (plannedByMe . snd)
    . concatMap (reserveAndExams . snd)
    . M.toList
    . slots
    . setSlotsOnExams
  where reserveAndExams slot =
          map (\exam -> (reserveInvigilator slot, exam))
              $ M.elems $ examsInSlot slot

-- | Für das ZPA wird eine JSON-Datei erzeugt, in der Form
--
-- @
-- [
--   {
--     "anCode": 347,
--     "date": "25.01.2017",
--     "time": "12:30",
--     "reserveInvigilator_id": 130,
--     "rooms": [
--       {
--         "number": "R0.006",
--         "invigilator_id": 128,
--         "reserveRoom": false,
--         "handicapCompensation": false,
--         "duration": 90
--       },
--       {
--         "number": "R3.014",
--         "invigilator_id": 129,
--         "reserveRoom": false,
--         "handicapCompensation": true,
--         "duration": 99
--       }
--     ]
--   }
-- ]
-- @
--
planToZPA :: Plan -> String
planToZPA = unpack . encodePretty' config . planToZPAExams
  where
    config = defConfig { confCompare = keyOrder [ "anCode"
                                                , "date"
                                                , "time"
                                                , "reserveInvigilator_id"
                                                -- , "rooms"
                                                ]
                       }
