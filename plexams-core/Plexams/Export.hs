{-# LANGUAGE DeriveGeneric #-}
module Plexams.Export
  ( planToMD
  , planToHTMLTable
  , planToJSONForZPA
  ) where

import           Data.Aeson
import           Data.List          (intercalate, transpose)
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe)
import           Data.Time          (Day)
import           Data.Time.Calendar
import           GHC.Exts           (groupWith)
import           GHC.Generics
import           Plexams.Types


-- | Erzeugt eine Markdown-Version des aktuellen Plans
planToMD :: Plan -> String
planToMD plan = undefined
{-
    "# Prüfungsplan " ++ semesterName (semesterConfig plan) ++ "\n\n"
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
-}

insideTag :: String -> String -> String
insideTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

planToHTMLTable :: Plan -> String
planToHTMLTable plan =
        before ++ planToHTMLTable' ++ unscheduledExamsToList ++ after
  where
    sName = semester $ semesterConfig plan
    before =    "<html><head><meta charset=\"utf-8\"/><title>Prüfungsplan "
                 ++ sName
                 ++ "</title><style>\ntable, th, td {\n"
                 ++ "border: 1px solid black;\n"
                 ++ "}\n"
                 ++ "i { font-size: 50%; }"
                 ++ "</style></head><body>"
                 ++ "<h1>Prüfungsplan "
                 ++ sName
                 ++ "</h1>\n"
    after = "</body></html>"
    planToHTMLTable' = let header = "" : (map show $ examDays $ semesterConfig plan)
                           columns =  slotsPerDay $ semesterConfig plan
                           showExams (idx@(d,s), slot) = insideTag "i" (show idx)
                                                       ++ show (map anCode $ examsInSlot slot)
                           slotsAsMatrix = zipWith (:) columns
                                             $ map (map showExams)
                                             $ groupWith (\((_,t),_) -> t) $ M.toAscList $ slots plan
                       in insideTag "table"
                            $ (insideTag "tr" $ concatMap (insideTag "td") header)
                              ++ concatMap (insideTag "tr" . concatMap (insideTag "td")) slotsAsMatrix
    unscheduledExamsToList = insideTag "ol"
            $ concatMap (insideTag "li" . toString) $ unscheduledExams plan
    toString exam = show (anCode exam) ++ " " ++ name exam
                    ++ " (" ++ personShortName (lecturer exam)  ++ ")"

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
planToZPAExams plan = undefined
{-
    concatMap
        (\day -> concatMap
            (\slot -> map
                (examToZPAExam (dateString day)
                               (timeString slot)
                               (fromMaybe 0 $ reserveInvigilator slot)
                ) $ examsInSlot slot
            ) $ slotsOfDay day) $ examDays plan
-}

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
