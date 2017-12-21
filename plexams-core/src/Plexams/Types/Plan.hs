{-# LANGUAGE DeriveGeneric #-}
module Plexams.Types.Plan
  ( Plan(..)
  , scheduledExams
  , allExams
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
  , studentsOverlaps
  ) where

import           Control.Arrow                ((***))
import           Data.Aeson                   (ToJSON, defaultOptions,
                                               genericToEncoding, toEncoding)
import           Data.List                    (partition, sortBy, (\\))
import qualified Data.Map                     as M
import           Data.Ord                     (Down (Down), comparing)
import qualified Data.Set                     as S
import           Data.Time.Calendar
import           GHC.Generics
import           Plexams.Types.Common
import           Plexams.Types.Constraints
import           Plexams.Types.Exam
import           Plexams.Types.Groups
import           Plexams.Types.Persons
import           Plexams.Types.SemesterConfig
import           Plexams.Types.Slots

data Plan = Plan
    { semesterConfig     :: SemesterConfig
    , slots              :: Slots
    , unscheduledExams   :: M.Map Ancode Exam  -- ^ Liste der Prüfungen die noch keinem Slot zugeordnet sind
                                              -- Ancode -> Exam
    , persons            :: Persons
    , constraints        :: Constraints
    , students           :: Students -- TODO: remove field
    , studentsExams      :: StudentsExams
    , handicaps          :: [Handicap]
    , invigilators       :: Invigilators
    , invigilatorsPerDay :: M.Map DayIndex ([PersonID],[PersonID])
    , initialPlan        :: [Exam]
    }
  deriving (Show, Eq,  Generic)

instance ToJSON Plan where
    toEncoding = genericToEncoding defaultOptions

scheduledExams :: Plan -> [Exam]
scheduledExams plan =
    concatMap (M.elems . examsInSlot . snd) $ M.toList $ slots plan

allExams :: Plan -> [Exam]
allExams plan = let plan' = setSlotsOnExams plan
                in M.elems (unscheduledExams plan') ++ scheduledExams plan'

isScheduledAncode :: Ancode -> Plan -> Bool
isScheduledAncode ancode =
  elem ancode
  . concatMap (M.keys . examsInSlot)
  . M.elems
  . slots

isUnscheduledAncode :: Ancode -> Plan -> Bool
isUnscheduledAncode ancode =
  elem ancode
  . M.keys
  . unscheduledExams

isUnknownExamAncode :: Ancode -> Plan -> Bool
isUnknownExamAncode ancode plan =
     not (isScheduledAncode ancode plan)
  && not (isUnscheduledAncode ancode plan)

mkAvailableRooms :: Plan -> [AvailableRoom] -> AvailableRooms
mkAvailableRooms _ [] = M.empty
mkAvailableRooms plan rooms' =
  let slots' = M.keys $ slots plan
      (handicapCompensationRooms', normalRooms') =
                                          partition availableRoomHandicap rooms'
      normalRooms =
        sortBy (comparing (Down . availableRoomMaxSeats)) normalRooms'
      handicapCompensationRooms =
        sortBy (comparing (Down . availableRoomMaxSeats))
                handicapCompensationRooms'
      (handicapCompensationRoomOdd, handicapCompensationRoomEven) =
          (map snd *** map snd)
          $  partition fst
          $ zip (concat $ repeat [True,False]) handicapCompensationRooms
      roomSlots' = roomSlots $ constraints plan
      -- normalRoomsAlways =
      --  filter (not . (`elem` M.keys roomSlots') . availableRoomName) normalRooms
      allAvailableRooms = M.fromList $ zip slots' $ concat
                        $ repeat [ (normalRooms, handicapCompensationRoomOdd)
                                 , (normalRooms, handicapCompensationRoomEven)]
      filterNotRoomID roomID' = filter ((/=roomID') . availableRoomName)
      removeRoomFromAllOtherSlots :: RoomID -> [(DayIndex, SlotIndex)] -> AvailableRooms
                      -> AvailableRooms
      removeRoomFromAllOtherSlots roomID' slots'' allRooms =
        foldr ( -- \s ->
                M.alter (maybe Nothing (\(nr, hr) -> Just
                          ( filterNotRoomID roomID' nr
                          , filterNotRoomID roomID' hr
                          )
                        ))
              ) allRooms
              $ slots' \\ slots''
   in foldr (\(r, sl) allRooms -> removeRoomFromAllOtherSlots r sl allRooms)
            allAvailableRooms $ M.toList roomSlots'

maxDayIndex :: Plan -> DayIndex
maxDayIndex = (\x->x-1) . length . examDays . semesterConfig

maxSlotIndex :: Plan -> SlotIndex
maxSlotIndex = (\x->x-1) . length . slotsPerDay . semesterConfig

examDateAsString :: Exam -> Plan -> String
examDateAsString exam plan =
  maybe "" (dateString . (examDays (semesterConfig plan)!!) . fst) $ slot exam

examSlotAsString :: Exam -> Plan -> String
examSlotAsString exam plan =
  maybe "" ((slotsPerDay (semesterConfig plan)!!) . snd) $ slot exam

dateString :: Day -> String
dateString day = let (y, m, d) = toGregorian day
                     showWith0 n = let s = show n
                                   in if n < 10 then "0"++s else s
                 in showWith0 d ++ "." ++ showWith0 m ++ "." ++ show y

setSlotsOnExams :: Plan -> Plan
setSlotsOnExams plan = plan
    { slots = M.mapWithKey addSlotKeyToExam $ slots plan
    , unscheduledExams = M.map (\e -> e { slot = Nothing })
                             $ unscheduledExams plan
    }

addSlotKeyToExam :: (DayIndex, SlotIndex) -> Slot -> Slot
addSlotKeyToExam k slot' =
    slot' { examsInSlot = M.map (addSlotKey k) $ examsInSlot slot' }
  where
    addSlotKey k' exam = exam { slot = Just k' }

studentsOverlaps :: Plan -> Overlaps
studentsOverlaps plan =
  let studentsExamsPairs = concatMap (mkPairs . S.elems . snd)
                         $ M.elems $ studentsExams plan
      mkPairs xs = [ (a, b) | a <- xs, b <- xs, a /= b ]
      allOverlaps = foldr insertPair M.empty studentsExamsPairs
      insertPair (ancode, ancode') = M.alter (insertAncode ancode') ancode
      insertAncode ancode' Nothing = Just (M.singleton ancode' 1)
      insertAncode ancode' (Just ancodes) =
        Just (M.alter (Just . maybe 1 (+1)) ancode' ancodes)

  in Overlaps (Group ALL Nothing Nothing Nothing) allOverlaps
