{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.SemesterConfig
  ( SemesterConfig(..)
  , nonGOSlots
  , SemesterConfigFiles(..)
  , examDaysAsStrings
  , slotsAsStringsForRoom
  , AvailableRoom(..)
  , AvailableRooms
      -- * Config
  , showSlot
  , allSlots
  )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , empty
                                                )
import           Data.Aeson                     ( ToJSON
                                                , defaultOptions
                                                , genericToEncoding
                                                )
import           Data.List                      ( elemIndex )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate    ( toWeekDate )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                , parseTimeM
                                                )
import           Data.Time.LocalTime            ( TimeOfDay(TimeOfDay) )
import qualified Data.Yaml                     as Y
import           GHC.Generics

import           Plexams.Types.Common

data SemesterConfigFiles = SemesterConfigFiles
  { initialPlanFile :: Maybe FilePath -- ^ Datei in der die Prüfungen für das Semester vom ZPA stehen
  , personsFile :: Maybe FilePath -- ^ Datei in der die Personen für das Semester vom ZPA stehen
  , planManipFile :: Maybe FilePath
  , handicapsFile :: Maybe FilePath
  , studentsRegsFile :: Maybe FilePath
  , roomsFile :: Maybe FilePath
  , constraintsFile :: Maybe FilePath
  , invigilatorsFile :: Maybe FilePath
  , invigilationsFile :: Maybe FilePath
  } deriving (Eq, Show, Generic)

instance Y.FromJSON SemesterConfigFiles where
  parseJSON (Y.Object v) =
    SemesterConfigFiles <$> v Y..: "initialPlan" <*> v Y..: "persons" <*>
    v Y..:? "planManip" <*>
    v Y..:? "handicaps" <*>
    v Y..:? "studentregs" <*>
    v Y..:? "rooms" <*>
    v Y..:? "constraints" <*>
    v Y..:? "invigilators" <*>
    v Y..:? "invigilations"
  parseJSON _ = empty

instance ToJSON SemesterConfigFiles where
  toEncoding = genericToEncoding defaultOptions

data SemesterConfig = SemesterConfig
  { semester :: Text -- ^ Semester
  , firstDay :: Day -- ^ Erster Tag des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 10@
  , lastDay :: Day -- ^ Letzter Tag  des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 21@
  , examsOnSaturdays :: Bool
  , examDays :: [Day] -- ^ vom ersten bis letzten Tag OHNE Wochenende
  , goSlots :: [(Int, Int)]
  , slotsPerDay :: [String] -- ^ Liste von Slots als Zeitstrings in der Form @HH:MM@. Ein Slot ist IMMER 120 Minuten lang
  , files :: SemesterConfigFiles
  , availableRooms :: [AvailableRoom]
  , importedExams :: [[Integer]]
  , goOtherExams :: [Ancode]
  , scheduleFrozen :: Bool
  } deriving (Eq, Show, Generic)

allSlots :: SemesterConfig -> [(Int, Int)]
allSlots semesterConfig =
  [ (d, s)
  | d <- [0 .. length (examDays semesterConfig) - 1]
  , s <- [0 .. length (slotsPerDay semesterConfig) - 1]
  ]

nonGOSlots :: SemesterConfig -> [(Int, Int)]
nonGOSlots semesterConfig =
  [ (d, s)
  | d <- [0 .. length (examDays semesterConfig) - 1]
  , s <- [0 .. length (slotsPerDay semesterConfig) - 1]
  , (d, s) `notElem` goSlots semesterConfig
  ]

showSlot :: SemesterConfig -> Maybe (DayIndex, SlotIndex) -> String
showSlot _ Nothing = ""
showSlot semesterConfig (Just (dayIdx, slotIdx)) =
  formatTime defaultTimeLocale "%d.%m.%y" (examDays semesterConfig !! dayIdx)
    ++ ", "
    ++ (slotsPerDay semesterConfig !! slotIdx)
    ++ " Uhr"

showExamDay :: Day -> String
showExamDay = formatTime defaultTimeLocale "%a, %d.%m.%y"

examDaysAsStrings :: SemesterConfig -> [String]
examDaysAsStrings = map showExamDay . examDays

slotsAsStringsForRoom :: SemesterConfig -> [String]
slotsAsStringsForRoom =
  map showSlotForRoom
    . mapMaybe (parseTimeM True defaultTimeLocale "%R")
    . slotsPerDay
 where
  showSlotForRoom (TimeOfDay h m _) = if m >= 15
    then showSlotForRoom' h (m - 15)
    else showSlotForRoom' (h - 1) (m + 45)
  showSlotForRoom' h m =
    show2d h ++ ":" ++ show2d m ++ " - " ++ show2d (h + 2) ++ ":" ++ show2d m
  show2d :: Int -> String
  show2d n | n <= 9    = "0" ++ show n
           | otherwise = show n

instance Y.FromJSON SemesterConfig where
  parseJSON (Y.Object v) =
    makeSemesterConfig <$> v Y..: "semester" <*> v Y..: "firstDay" <*>
    v Y..: "lastDay" <*>
    v Y..:? "examsOnSaturdays" Y..!= False <*>
    v Y..: "goDay0" <*>
    v Y..:? "goNotOnDays" Y..!= [] <*>
    v Y..: "slotsPerDay" <*>
    v Y..: "files" <*>
    v Y..: "rooms" <*>
    v Y..: "notPlannedByMe" <*>
    v Y..: "goOtherExams" <*>
    v Y..:? "scheduleFrozen" Y..!= False
  parseJSON _ = empty

instance ToJSON SemesterConfig where
  toEncoding = genericToEncoding defaultOptions

makeSemesterConfig
  :: Text
  -> String
  -> String
  -> Bool
  -> String
  -> [Int]
  -> [String]
  -> SemesterConfigFiles
  -> [AvailableRoom]
  -> [[Integer]]
  -> [Ancode]
  -> Bool
  -> SemesterConfig
makeSemesterConfig s f l examsOnSaturdays' goDay0 goNotOnDays =
  SemesterConfig s firstDay' lastDay' examsOnSaturdays' realExamDays goSlots'
 where
  makeDay :: String -> Day
  makeDay str = fromMaybe (error $ "cannot parse date: " ++ str)
                          (parseTimeM True defaultTimeLocale "%d.%m.%Y" str)
  firstDay'    = makeDay f
  lastDay'     = makeDay l
  realExamDays = filter (notWeekend . toWeekDate) [firstDay' .. lastDay']
  notWeekend (_, _, weekday) = weekday <= if examsOnSaturdays' then 6 else 5
  goDay0Index = fromMaybe 0 $ elemIndex (makeDay goDay0) realExamDays -- BUG
  goSlots' =
    filter ((>= 0) . fst) $ map (\(d, t) -> (d + goDay0Index, t)) $ filter
      (not . (`elem` goNotOnDays) . fst)
      rawGOSlots
  rawGOSlots =
    [ (-3, 0)
    , (-3, 1)
    , (-1, 3)
    , (-1, 4)
    , (-1, 5)
    , (0 , 0)
    , (0 , 1)
    , (1 , 3)
    , (1 , 4)
    , (1 , 5)
    , (2 , 0)
    , (2 , 1)
    , (3 , 3)
    , (3 , 4)
    , (3 , 5)
    , (4 , 0)
    , (4 , 1)
    , (5 , 3)
    , (5 , 4)
    , (5 , 5)
    , (6 , 0)
    , (6 , 1)
    , (9 , 3)
    , (9 , 4)
    , (9 , 5)
    ]

data AvailableRoom = AvailableRoom
  { availableRoomName :: String
  , availableRoomMaxSeats :: Integer
  , availableRoomHandicap :: Bool
  , availableRoomNeedsRequest :: Bool
  } deriving (Eq, Show, Generic)

instance Y.FromJSON AvailableRoom where
  parseJSON (Y.Object v) =
    AvailableRoom <$> v Y..: "name" <*> v Y..: "seats" <*>
    v Y..:? "handicap" Y..!= False <*>
    v Y..:? "needsRequest" Y..!= False
  parseJSON _ = empty

instance ToJSON AvailableRoom where
  toEncoding = genericToEncoding defaultOptions

type AvailableRooms
   = M.Map (DayIndex, SlotIndex)
                            -- ( Normale Räume (absteigend sortiert nach Größe)
                            -- , Handicap Räume)
      ([AvailableRoom], [AvailableRoom], [AvailableRoom])
