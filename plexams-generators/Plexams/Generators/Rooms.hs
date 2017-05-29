module Plexams.Generators.Rooms
  ( generateRooms
  ) where

import           Control.Arrow (second)
import           Data.List     (sortBy)
import qualified Data.Map      as M
import           Data.Ord      (Down (Down), comparing)
import           GHC.Exts      (sortWith)
import           Plexams.Types

generateRooms :: Plan -> (Plan, [AddRoomToExam])
generateRooms plan =
  let availableRooms' = map (second fst)
                      $ M.toList $ mkAvailableRooms plan
                                    (availableRooms $ semesterConfig plan)
      slots' = M.toList $ M.map (M.elems . examsInSlot) $ slots plan
      roomsAndSlots = [ (s, (rooms, filter plannedByMe exams))
                      | (s,  rooms) <- availableRooms'
                      , (s', exams) <- slots'
                      , s == s'
                      ]
      slotsWithRooms = map (second (uncurry setRoomsOnSlot)) roomsAndSlots
  in ( plan { slots = foldr (\(s,(exams, _)) slots'' ->
                              M.alter (\(Just slot)
                                        -> Just $ slot {
                                            examsInSlot = M.fromList
                                              $ map (\e -> (anCode e, e))
                                                    exams
                                          }
                                      ) s slots''
                            )
                            (slots plan)
                            slotsWithRooms
            }
     , sortWith addRoomAnCode $ concatMap (snd . snd) slotsWithRooms
     )

setRoomsOnSlot :: [AvailableRoom] -> [Exam] -> ([Exam], [AddRoomToExam])
setRoomsOnSlot r e = setRoomsOnSlot' r e []

setRoomsOnSlot' :: [AvailableRoom] -> [Exam] -> [AddRoomToExam]
               -> ([Exam], [AddRoomToExam])
setRoomsOnSlot' [] _ _ = error "no rooms left for slot"
setRoomsOnSlot' (room:rooms) exams planManips =
  let exams' = sortBy (comparing (Down . seatsMissing)) exams
  in case exams' of
    [] -> (exams, planManips)
    (nextExam:sortedExams) ->
     if seatsMissing nextExam == 0
     then (sortedExams, planManips)
     else let (exam', planManip) = setRoomOnExam nextExam room
       in setRoomsOnSlot' rooms (exam' : sortedExams) (planManip : planManips)

setRoomOnExam :: Exam -> AvailableRoom -> (Exam, AddRoomToExam)
setRoomOnExam exam availableRoom  =
  let neededSeats = seatsMissing exam
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
