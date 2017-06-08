module Plexams.Generators.Rooms
  ( generateRooms
  ) where

import           Control.Arrow (second)
import           Data.List     (partition, sortBy)
import qualified Data.Map      as M
import           Data.Ord      (Down (Down), comparing)
import           GHC.Exts      (sortWith)
import           Plexams.Types

generateRooms :: Plan -> [AddRoomToExam]
generateRooms plan =
  let availableRooms' =  M.toList $ mkAvailableRooms plan
                                    (availableRooms $ semesterConfig plan)
      slots' = M.toList $ M.map (M.elems . examsInSlot) $ slots plan
      roomsAndSlots = [ (s, (rooms', filter plannedByMe exams'))
                      | (s,  rooms') <- availableRooms'
                      , (s', exams') <- slots'
                      , s == s'
                      ]
      slotsWithRooms = map (second (uncurry setRoomsOnSlot)) roomsAndSlots
  in sortWith addRoomAnCode $ concatMap snd slotsWithRooms

setRoomsOnSlot :: ([AvailableRoom], [AvailableRoom]) -> [Exam]
               -> [AddRoomToExam]
setRoomsOnSlot (n,h) e =
  let (_, planManipsNormal) = setNormalRoomsOnSlot n e []
      (_, planManipsHandicap) = setHandicapRoomsOnSlot h e
  in planManipsNormal ++ planManipsHandicap

seatsMissing' :: Exam -> Integer
seatsMissing' exam = seatsMissing exam -
                           toInteger (length (studentsWithHandicaps exam))


setNormalRoomsOnSlot :: [AvailableRoom] -> [Exam] -> [AddRoomToExam]
               -> ([Exam], [AddRoomToExam])
setNormalRoomsOnSlot [] _ _ = error "no normal rooms left for slot"
setNormalRoomsOnSlot (room:rooms') exams' planManips =
  let exams'' = sortBy (comparing (Down . seatsMissing')) exams'
  in case exams'' of
    [] -> (exams', planManips)
    (nextExam:sortedExams) ->
     if seatsMissing' nextExam <= 0
     then (sortedExams, planManips)
     else let (exam', planManip) = setRoomOnExam nextExam room
       in setNormalRoomsOnSlot rooms' (exam' : sortedExams)
                                          (planManip : planManips)

setRoomOnExam :: Exam -> AvailableRoom -> (Exam, AddRoomToExam)
setRoomOnExam exam availableRoom  =
  let neededSeats = seatsMissing' exam
      roomName = availableRoomName availableRoom
      seatsPlanned' = min neededSeats
                          $ availableRoomMaxSeats availableRoom
  in (exam { rooms = Room
                  { roomID = roomName
                  , maxSeats = availableRoomMaxSeats availableRoom
                  , deltaDuration = 0
                  , invigilator = Nothing
                  , reserveRoom = neededSeats <= 3
                                  && registrations exam /= neededSeats
                  , handicapCompensation = False
                  , seatsPlanned = seatsPlanned'
                  }
               : rooms exam
         }
      , AddRoomToExam (anCode exam) roomName seatsPlanned' Nothing
      )

setHandicapRoomsOnSlot :: [AvailableRoom] -> [Exam]
               -> ([Exam], [AddRoomToExam])
setHandicapRoomsOnSlot [] _ = error "no handicap rooms left for slot"
setHandicapRoomsOnSlot (room:rooms') exams'
  | not (any withHandicaps exams') = (exams', [])
  | otherwise =
  let examsWithHandicaps = filter withHandicaps exams'
      (studentsOwnRoom, otherStudents) =
          partition needsRoomAlone
          $ concatMap studentsWithHandicaps examsWithHandicaps
      (exams'', planManips) =
        if length otherStudents <= fromInteger (availableRoomMaxSeats room)
        then setHandicapRoomOnAllExams room exams'
        else error "too much handicap students, room to small"
      (exams''', planManips')
        | null studentsOwnRoom = (exams'', [])
        | length studentsOwnRoom <= 1 = setHandicapsRoomAlone (head rooms') exams''
        | otherwise =  error "more than one students needs room for its own"
  in (exams''', planManips ++ planManips')

setHandicapRoomOnAllExams :: AvailableRoom -> [Exam] -> ([Exam],[AddRoomToExam])
setHandicapRoomOnAllExams room  = second concat
                                . unzip
                                . map (setHandicapRoomOnExam room)
  where
    setHandicapRoomOnExam availableRoom exam =
      let studentsWithHandicaps' = filter (not . needsRoomAlone)
                                               (studentsWithHandicaps exam)
          seatsPlanned' = toInteger (length studentsWithHandicaps')
          deltaDuration'  = (duration exam  * maximum
            (map deltaDurationPercent studentsWithHandicaps')) `div` 100
          roomName = availableRoomName availableRoom
      in if withHandicaps exam && not (null studentsWithHandicaps')
        then
        (exam { rooms = Room
                { roomID = roomName
                , maxSeats = availableRoomMaxSeats availableRoom
                , deltaDuration = deltaDuration'
                , invigilator = Nothing
                , reserveRoom = False
                , handicapCompensation = True
                , seatsPlanned = seatsPlanned'
                }
                   : rooms exam
             }
          , [AddRoomToExam (anCode exam)
                          roomName
                          seatsPlanned'
                          (Just deltaDuration')]
          )
        else (exam,[])

setHandicapsRoomAlone :: AvailableRoom -> [Exam] -> ([Exam],[AddRoomToExam])
setHandicapsRoomAlone availableRoom exams' =
  let ([exam], others) = partition (any needsRoomAlone . studentsWithHandicaps)
                                   exams'
      roomName = availableRoomName availableRoom
      deltaDuration'  = (duration exam  *
        deltaDurationPercent
        ( head $ filter needsRoomAlone
        $ studentsWithHandicaps exam)) `div` 100
      (exam', addRoomToExam) =
        ( exam { rooms = Room
            { roomID = roomName
            , maxSeats = availableRoomMaxSeats availableRoom
            , deltaDuration = deltaDuration'
            , invigilator = Nothing
            , reserveRoom = False
            , handicapCompensation = True
            , seatsPlanned = 1
            } : rooms exam
          }
        , [AddRoomToExam (anCode exam)
                         roomName
                         1
                         (Just deltaDuration')]
        )
  in (exam' : others, addRoomToExam)
