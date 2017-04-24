{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export
  ( planToMD
  , planToHTMLTable
  , planToZPA
  , semesterConfigAsString
  , exportPlanManips
  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (intercalate, partition, transpose)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Time                  (Day)
import           Data.Time.Calendar
import qualified Data.Vector                as V
import           GHC.Exts                   (groupWith, sortWith)
import           GHC.Generics
import           Plexams.Query              (allExams)
import           Plexams.Types


-- | TODO: Erzeugt eine Markdown-Version des aktuellen Plans
planToMD :: Plan -> String
planToMD = ("-   "++)
          . intercalate "\n-   "
          . map show
          . allExams
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
        before
        ++ planToHTMLTable'
        ++ insideTag "h2" "Noch zu planen"
        ++ unscheduledExamsToList
        ++ insideTag "h2" "Nicht von mir zu planen"
        ++ unscheduledExamsPlannedByOthers
        ++ after
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
    planToHTMLTable' =
      let header = "" : map show (examDays $ semesterConfig plan)
          columns =  slotsPerDay $ semesterConfig plan
          showExams (idx@(d,s), slot) = insideTag "i" (show idx)
              ++ show (map anCode $ M.elems $ examsInSlot slot)
          slotsAsMatrix = zipWith (:) columns
               $ map (map showExams)
               $ groupWith (\((_,t),_) -> t) $ M.toAscList $ slots plan
      in insideTag "table"
          $ insideTag "tr" (concatMap (insideTag "td") header)
            ++ concatMap (insideTag "tr" . concatMap (insideTag "td"))
                         slotsAsMatrix
    (unscheduledExams', plannedByOtherExams) =
        partition plannedByMe $ M.elems $ unscheduledExams plan
    unscheduledExamsToList = insideTag "ol"
            $ concatMap (insideTag "li" . toString) unscheduledExams'
    unscheduledExamsPlannedByOthers = insideTag "ul"
            $ concatMap (insideTag "li" . toString) plannedByOtherExams
    toString exam = show (anCode exam) ++ " " ++ name exam
                    ++ " (" ++ personShortName (lecturer exam)  ++ ")"

dateString :: Day -> String
dateString day = let (y, m, d) = toGregorian day
                     showWith0 n = let s = show n
                                   in if n < 10 then "0"++s else s
                 in showWith0 d ++ "." ++ showWith0 m ++ "." ++ show y

--------------------------------------------------------------------------------
-- Export for ZPA
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Print SemesterConfig
--------------------------------------------------------------------------------

instance ToJSON SemesterConfig where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON AvailableRoom where
    toEncoding = genericToEncoding defaultOptions

semesterConfigAsString :: Plan -> String
semesterConfigAsString = unpack . encodePretty . semesterConfig

--------------------------------------------------------------------------------
-- Export PlanManips to Yaml
--------------------------------------------------------------------------------

exportPlanManips :: [PlanManip] -> String
exportPlanManips = intercalate "\n" . map exportPlanManip

exportPlanManip :: PlanManip -> String
exportPlanManip (AddExamToSlot a d s) =
  "- [" ++ show a ++ ", " ++ show d ++ ", " ++ show s ++ "]"
