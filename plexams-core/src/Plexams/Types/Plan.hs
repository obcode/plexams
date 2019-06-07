{-# LANGUAGE DeriveGeneric #-}

module Plexams.Types.Plan
  ( Plan(..)
  , scheduledExams
  , allExams
  , allExamsPlannedByMe
  , isScheduledAncode
  , isUnscheduledAncode
  , isUnknownExamAncode
  , mkAvailableRooms
  , maxDayIndex
  , maxSlotIndex
  , examDateAsString
  , examSlotAsString
  , setSlotsOnExams
  , dateString
  )
where

import           Data.Aeson                     ( ToJSON
                                                , defaultOptions
                                                , genericToEncoding
                                                , toEncoding
                                                )
import           Data.List                      ( (\\)
                                                , partition
                                                , sortOn
                                                )
import qualified Data.Map                      as M
import           Data.Ord                       ( Down(Down) )
import           Data.Time.Calendar
import           GHC.Generics
import           Plexams.Types.Common
import           Plexams.Types.Constraints
import           Plexams.Types.Exam
import           Plexams.Types.Persons
import           Plexams.Types.SemesterConfig
import           Plexams.Types.Slots

data Plan = Plan
  { semesterConfig :: SemesterConfig
  , slots :: Slots
  , unscheduledExams :: M.Map Ancode Exam
  , persons :: Persons
  , constraints :: Constraints
  , invigilators :: Invigilators
  , invigilatorsPerDay :: M.Map DayIndex ([PersonID], [PersonID])
  , initialPlan :: [Exam]
  } deriving (Show, Eq, Generic)

instance ToJSON Plan where
  toEncoding = genericToEncoding defaultOptions

scheduledExams :: Plan -> [Exam]
scheduledExams plan = concatMap (M.elems . examsInSlot) $ M.elems $ slots plan

allExams :: Plan -> [Exam]
allExams plan =
  let plan' = setSlotsOnExams plan
  in  M.elems (unscheduledExams plan') ++ scheduledExams plan'

allExamsPlannedByMe :: Plan -> [Exam]
allExamsPlannedByMe = filter plannedByMe . allExams

isScheduledAncode :: Ancode -> Plan -> Bool
isScheduledAncode ancode =
  elem ancode . concatMap (M.keys . examsInSlot) . M.elems . slots

isUnscheduledAncode :: Ancode -> Plan -> Bool
isUnscheduledAncode ancode = elem ancode . M.keys . unscheduledExams

isUnknownExamAncode :: Ancode -> Plan -> Bool
isUnknownExamAncode ancode plan =
  not (isScheduledAncode ancode plan) && not (isUnscheduledAncode ancode plan)

mkAvailableRooms :: Plan -> [AvailableRoom] -> AvailableRooms
mkAvailableRooms _ [] = M.empty
mkAvailableRooms plan rooms'
  = let
      slots' = M.keys $ slots plan
      (handicapCompensationRooms', normalRooms') =
        partition availableRoomHandicap rooms'
      (normalRoomsNeedRequest', normalOwnRooms') =
        partition availableRoomNeedsRequest normalRooms'
      normalRoomsNeedRequest =
        sortOn availableRoomMaxSeats normalRoomsNeedRequest'
      normalOwnRooms = sortOn (Down . availableRoomMaxSeats) normalOwnRooms'
      handicapCompensationRooms =
        sortOn availableRoomMaxSeats handicapCompensationRooms'
      roomSlots'        = roomSlots $ constraints plan
      allAvailableRooms = M.fromList $ zip slots' $ concat $ repeat
        [(normalOwnRooms, normalRoomsNeedRequest, handicapCompensationRooms)]
      filterNotRoomID roomID' = filter ((/= roomID') . availableRoomName)
      removeRoomFromAllOtherSlots
        :: RoomID -> [(DayIndex, SlotIndex)] -> AvailableRooms -> AvailableRooms
      removeRoomFromAllOtherSlots roomID' slots'' allRooms =
        foldr -- \s ->
            (M.alter
              (maybe
                Nothing
                (\(nr, nrR, hr) -> Just
                  ( filterNotRoomID roomID' nr
                  , filterNotRoomID roomID' nrR
                  , filterNotRoomID roomID' hr
                  )
                )
              )
            )
            allRooms
          $  slots'
          \\ slots''
    in
      foldr (\(r, sl) allRooms -> removeRoomFromAllOtherSlots r sl allRooms)
            allAvailableRooms
        $ M.toList roomSlots'

maxDayIndex :: Plan -> DayIndex
maxDayIndex = (\x -> x - 1) . length . examDays . semesterConfig

maxSlotIndex :: Plan -> SlotIndex
maxSlotIndex = (\x -> x - 1) . length . slotsPerDay . semesterConfig

examDateAsString :: Exam -> Plan -> String
examDateAsString exam plan =
  maybe "" (dateString . (examDays (semesterConfig plan) !!) . fst) $ slot exam

examSlotAsString :: Exam -> Plan -> String
examSlotAsString exam plan =
  maybe "" ((slotsPerDay (semesterConfig plan) !!) . snd) $ slot exam

dateString :: Day -> String
dateString day =
  let (y, m, d) = toGregorian day
      showWith0 n = let s = show n in if n < 10 then "0" ++ s else s
  in  showWith0 d ++ "." ++ showWith0 m ++ "." ++ show y

setSlotsOnExams :: Plan -> Plan
setSlotsOnExams plan = plan
  { slots            = M.mapWithKey addSlotKeyToExam $ slots plan
  , unscheduledExams = M.map (\e -> e { slot = Nothing })
                         $ unscheduledExams plan
  }

addSlotKeyToExam :: (DayIndex, SlotIndex) -> Slot -> Slot
addSlotKeyToExam k slot' = slot'
  { examsInSlot = M.map (addSlotKey k) $ examsInSlot slot'
  }
  where addSlotKey k' exam = exam { slot = Just k' }
