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
import           Data.List                  (intercalate, nub, partition,
                                             sortBy, transpose)
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
planToMD plan =
          -- ("-   "++)
          -- . intercalate "\n-   "
          -- . map show
          -- . allExams
    "# Prüfungsplan " ++ semester (semesterConfig plan) ++ "\n\n"
    ++ intercalate "\n\n" (map slotToMD slotList)
    ++ "\n\n## Noch nicht geplante (sortiert nach Anmeldezahlen)\n\n"
    ++ intercalate "\n\n"
                  (map examToMD $ unscheduledExamsSortedByRegistrations plan)

  where
    slotList = M.toAscList $ slots plan
    slotToMD (ds, slot) = dayAndSlotToString ds ++ "\n\n"
      ++ intercalate "\n\n" (map (("    "++) . examToMD) $ M.elems $ examsInSlot slot)
    dayAndSlotToString (d,s) = "- " ++ dayStr ++ ": " ++ slotStr ++ " Uhr"
      where dayStr = show $ (!!d) $ examDays $ semesterConfig plan
            slotStr = (!!s) $ slotsPerDay $ semesterConfig plan
    examToMD exam = "- " ++ show exam

unscheduledExamsSortedByRegistrations :: Plan -> [Exam]
unscheduledExamsSortedByRegistrations =
                sortBy (\e1 e2 -> compare (registrations e2)
                                           (registrations e1))
                . filter plannedByMe
                . M.elems
                . unscheduledExams

insideTag :: String -> String -> String
insideTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

planToHTMLTable :: Maybe [Ancode] -> Plan -> String
planToHTMLTable maybeExams plan =
        before
        ++ planToHTMLTable'
        ++ caption
        ++ insideTag "h2" "Noch zu planen"
        ++ unscheduledExamsToList
        ++ insideTag "h2" "Nicht von mir zu planen"
        ++ unscheduledExamsPlannedByOthers
        ++ after
  where
    sName = semester $ semesterConfig plan
    before =
      "<html><head><meta charset=\"utf-8\"/><title>Prüfungsplan "
     ++ sName
     ++ "</title>"
     ++ "<script type=\"text/javascript\" src=\"plexams.js\"></script>"
     ++ "<link href=\"plexams.css\" rel=\"stylesheet\" type=\"text/css\" />"
     ++ "</head><body>"
     ++ "<h1>Prüfungsplan "
     ++ sName
     ++ "</h1>\n"
    after = "<script src=\"https://code.jquery.com/jquery-latest.js\"></script>"
         ++ "<script>"
         ++ "$(\".tiptext\").mouseover(function() {\n"
         ++ "  $(this).children(\".description\").show();\n"
         ++ "}).mouseout(function() {\n"
         ++ "    $(this).children(\".description\").hide();\n"
         ++ "});\n"
         ++ "</script>"
         ++"</body></html>"
    caption = "<span class=\"tiptext\">Basisfarbe</span>\n"
           ++ "<span class=\"tiptext reExam\">Wiederholung</span>\n"
           ++ "<span class=\"tiptext activeExam\">Aktive Prüfung</span>\n"
           ++ "<span class=\"tiptext conflict\">Konflikte</span>\n"
           ++ concatMap ((\g -> "<span class=\"tiptext "++g++"\">"++g++"</span>\n")
                  . show) allDegrees

    planToHTMLTable' =
      let header = "" : map show (examDays $ semesterConfig plan)
          columns =  slotsPerDay $ semesterConfig plan
          showExams (idx@(d,s), slot) = insideTag "i" (show idx)
              ++ concatMap showExam (M.elems $ examsInSlot slot)
          showExam exam = "<div class=\"tiptext"
                ++ (if reExam exam then " reExam " else " ")
                ++ unwords (nub $ map (show . groupDegree) (groups exam))
                ++ (if isActiveExam exam then " activeExam " else "")
                ++ (if isConflict exam then " conflict " else "")
                ++ "\">"
                ++ show (anCode exam)
                ++ "(" ++ show (registrations exam) ++ ")"
                ++ "<div class=\"description\">"
                ++ show exam
                ++ "</div></div>"
          isActiveExam exam = maybe False (elem $ anCode exam) maybeExams
          isConflict exam = anCode exam `elem` conflicts
          conflicts = maybe [] (concatMap mkConflicts) maybeExams
          mkConflicts ancode = maybe []
            ( concatMap ( M.keys
                          . M.findWithDefault M.empty ancode
                          . olOverlaps
                        )
              . overlaps
            )
            $ constraints plan
          slotsAsMatrix = zipWith (:) columns
               $ map (map showExams)
               $ groupWith (\((_,t),_) -> t) $ M.toAscList $ slots plan
      in insideTag "table"
          $ insideTag "tr" (concatMap (insideTag "td") header)
            ++ concatMap (insideTag "tr" . concatMap (insideTag "td"))
                         slotsAsMatrix
    plannedByOtherExams =
        filter (not . plannedByMe) $ M.elems $ unscheduledExams plan
    unscheduledExamsToList = insideTag "ol"
            $ concatMap (insideTag "li" . show)
            (unscheduledExamsSortedByRegistrations plan)
    unscheduledExamsPlannedByOthers = insideTag "ul"
            $ concatMap (insideTag "li" . show) plannedByOtherExams
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
