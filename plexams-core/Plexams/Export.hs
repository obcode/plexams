{-# LANGUAGE DeriveGeneric #-}
module Plexams.Export
  ( planToMD
  , planToHTMLTable
  , planToJSONForZPA
  ) where

import           Data.Aeson
import           Data.Maybe    (fromMaybe)
import           GHC.Generics
import           Plexams.Types


-- | Erzeugt eine Markdown-Version des aktuellen Plans
planToMD :: Plan -> String
planToMD plan =
    "# Prüfungsplan " ++ semesterName plan ++ "\n\n"
    ++ concatMap dayMD (realExamDays plan)
    ++ "## Noch nicht eingeplante Prüfungen\n\n"
    ++ concatMap (\eStr -> "-   " ++ eStr ++ "\n\n")
                 (map examMD (unscheduledExams plan))
  where
    dayMD examDay = "## " ++ dateString examDay ++ "\n\n"
                    ++ concatMap slotMD (slotsOfDay examDay)
    slotMD slot = "- " ++ timeString slot ++ "\n\n"
    examMD exam = show (anCode exam) ++ " " ++ name exam
                    ++ " (" ++ personShortName (lecturer exam)  ++ ")"

insideTag :: String -> String -> String
insideTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

planToHTMLTable :: Plan -> String
planToHTMLTable plan = before ++ planToHTMLTable' ++ after
  where
    before =    "<html><head><meta charset=\"utf-8\"/><title>Prüfungsplan "
                 ++ semesterName plan
                 ++ "</title><style>\ntable, th, td {\n"
                 ++ "border: 1px solid black;\n"
                 ++ "}\n"
                 ++ "</style></head><body>"
                 ++ "<h1>Prüfungsplan "
                 ++ semesterName plan
                 ++ "</h1>\n"
    after = "</body></html>"
    planToHTMLTable' = undefined


data ZPAExam = ZPAExam
    { zpaExamAnCode               :: Integer
    , zpaExamDate                 :: String
    , zpaExamTime                 :: String
    , zpaExamReserveInvigilatorId :: Integer
    , zpaExamRooms                :: [ZPARoom]
    }
  deriving (Generic)

instance ToJSON ZPAExam where
    toEncoding = genericToEncoding defaultOptions

examToZPAExam :: String -> String -> Integer -> Exam -> ZPAExam
examToZPAExam date time reserveInvigilator exam = ZPAExam
    { zpaExamAnCode = anCode exam
    , zpaExamDate = date
    , zpaExamTime = time
    , zpaExamReserveInvigilatorId = reserveInvigilator
    , zpaExamRooms = map (roomToZPARoom $ duration exam) $ rooms exam
    }

data ZPARoom = ZPARoom
    { zpaRoomNumber               :: String
    , zpaRoomInvigilatorId        :: Integer
    , zpaRoomReserveRoom          :: Bool
    , zpaRoomHandicapCompensation :: Bool
    , zpaRoomDuration             :: Integer
    }
  deriving (Generic)

instance ToJSON ZPARoom where
    toEncoding = genericToEncoding defaultOptions

roomToZPARoom :: Integer -> Room -> ZPARoom
roomToZPARoom duration room = ZPARoom
    { zpaRoomNumber = roomID room
    , zpaRoomInvigilatorId = fromMaybe 0 $ invigilator room
    , zpaRoomReserveRoom = reserveRoom room
    , zpaRoomHandicapCompensation = handicapCompensation room
    , zpaRoomDuration = duration + deltaDuration room
    }

planToZPAExams :: Plan -> [ZPAExam]
planToZPAExams plan =
    concatMap
        (\day -> concatMap
            (\slot -> map
                (examToZPAExam (dateString day)
                               (timeString slot)
                               (fromMaybe 0 $ reserveInvigilator slot)
                ) $ examsInSlot slot
            ) $ slotsOfDay day) $ examDays plan

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
planToJSONForZPA :: Plan -> Encoding
planToJSONForZPA = undefined
