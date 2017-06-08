module Plexams.Generators.Rooms
  ( generateRooms
  ) where

import           Control.Arrow        (second)
import           Control.Monad.Writer
import           Data.List            (partition, sortBy)
import qualified Data.Map             as M
import           Data.Ord             (Down (Down), comparing)
import           GHC.Exts             (sortWith)
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
  snd $ runWriter $ do
    _ <- setNormalRoomsOnSlot n e
    setHandicapRoomsOnSlot h e

seatsMissing' :: Exam -> Integer
seatsMissing' exam = seatsMissing exam -
                           toInteger (length (studentsWithHandicaps exam))

setNormalRoomsOnSlot :: [AvailableRoom] -> [Exam]
                     -> Writer [AddRoomToExam] [Exam]
setNormalRoomsOnSlot [] _ = error "no normal rooms left for slot"
setNormalRoomsOnSlot (room:rooms') exams' = do
  let exams'' = sortBy (comparing (Down . seatsMissing')) exams'
  case exams'' of
    [] -> return exams'
    (nextExam:sortedExams) ->
     if seatsMissing' nextExam <= 0
     then return sortedExams
     else do
       exam' <- setRoomOnExam nextExam room
       setNormalRoomsOnSlot rooms' (exam' : sortedExams)

setRoomOnExam :: Exam -> AvailableRoom -> Writer [AddRoomToExam] Exam
setRoomOnExam exam availableRoom  = do
  let neededSeats = seatsMissing' exam
      roomName = availableRoomName availableRoom
      seatsPlanned' = min neededSeats
                          $ availableRoomMaxSeats availableRoom
  tell [AddRoomToExam (anCode exam) roomName seatsPlanned' Nothing]
  return exam { rooms = Room
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

setHandicapRoomsOnSlot :: [AvailableRoom] -> [Exam]
               -> Writer [AddRoomToExam] ()
setHandicapRoomsOnSlot [] _ = error "no handicap rooms left for slot"
setHandicapRoomsOnSlot (room:rooms') exams'
  | not (any withHandicaps exams') = return ()
  | otherwise = do
      let examsWithHandicaps = filter withHandicaps exams'
          (studentsOwnRoom, otherStudents) =
              partition needsRoomAlone
              $ concatMap studentsWithHandicaps examsWithHandicaps
      if length otherStudents <= fromInteger (availableRoomMaxSeats room)
        then setHandicapRoomOnAllExams room exams'
        else error "too much handicap students, room to small"
      unless (null studentsOwnRoom) $
        if length studentsOwnRoom <= 1
          then setHandicapsRoomAlone (head rooms') exams'
          else error "more than one students needs room for its own"
      return ()

setHandicapRoomOnAllExams :: AvailableRoom -> [Exam]
                          -> Writer [AddRoomToExam] ()
setHandicapRoomOnAllExams room  = mapM_ (setHandicapRoomOnExam room)
  where
    setHandicapRoomOnExam :: AvailableRoom -> Exam -> Writer [AddRoomToExam] ()
    setHandicapRoomOnExam availableRoom exam = do
      let studentsWithHandicaps' = filter (not . needsRoomAlone)
                                               (studentsWithHandicaps exam)
          seatsPlanned' = toInteger (length studentsWithHandicaps')
          deltaDuration'  = (duration exam  * maximum
            (map deltaDurationPercent studentsWithHandicaps')) `div` 100
          roomName = availableRoomName availableRoom
      when (withHandicaps exam && not (null studentsWithHandicaps')) $
        tell [AddRoomToExam (anCode exam)
                            roomName
                            seatsPlanned'
                            (Just deltaDuration')]
      return ()

setHandicapsRoomAlone :: AvailableRoom -> [Exam] -> Writer [AddRoomToExam] ()
setHandicapsRoomAlone availableRoom exams' = do
  let [exam] = filter (any needsRoomAlone . studentsWithHandicaps) exams'
      roomName = availableRoomName availableRoom
      deltaDuration'  = (duration exam  *
        deltaDurationPercent
        ( head $ filter needsRoomAlone
        $ studentsWithHandicaps exam)) `div` 100
  tell [AddRoomToExam (anCode exam) roomName 1 (Just deltaDuration')]
  return ()
