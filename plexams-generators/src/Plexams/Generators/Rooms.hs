module Plexams.Generators.Rooms
  ( generateRooms
  ) where

import           Control.Arrow        (second)
import           Control.Monad.Writer
import           Data.List            (partition, sortBy, splitAt)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust, isNothing)
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
seatsMissing' exam = seatsMissing exam - toInteger (length (handicapStudents exam))

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
  let (studsWithoutHandicap, studsWithHandicap) =
          partition (isNothing . studentHandicap) $ registeredStudents exam
      (studsForRoom, restOfStuds) =
        splitAt (fromInteger $ availableRoomMaxSeats availableRoom) studsWithoutHandicap
      roomName = availableRoomName availableRoom
  tell [AddRoomToExam (anCode exam) roomName (map studentMtknr studsForRoom) Nothing]
  return exam { rooms = Room
                  { roomID = roomName
                  , maxSeats = availableRoomMaxSeats availableRoom
                  , deltaDuration = 0
                  , invigilator = Nothing
                  , reserveRoom = length studsForRoom
                                  < fromInteger (registrations exam `div` 10)
                                --  && registrations exam /= neededSeats
                  , handicapCompensation = False
                  , studentsInRoom = studsForRoom
                  } : rooms exam
              , registeredStudents = restOfStuds ++ studsWithHandicap
              }

setHandicapRoomsOnSlot :: [AvailableRoom] -> [Exam]
               -> Writer [AddRoomToExam] ()
setHandicapRoomsOnSlot [] _ = error "no handicap rooms left for slot"
setHandicapRoomsOnSlot (room:rooms') exams'
  | not (any withHandicaps exams') = return ()
  | otherwise = do
      let examsWithHandicaps = filter withHandicaps exams'
          (studentsOwnRoom, otherStudents) =
              partition (handicapNeedsRoomAlone . fromJust . studentHandicap)
              $ concatMap handicapStudents examsWithHandicaps
      if length otherStudents <= fromInteger (availableRoomMaxSeats room)
        then setHandicapRoomOnAllExams room exams'
        else error "too much handicap students, room to small"
      unless (null studentsOwnRoom) $
        if length studentsOwnRoom == 1
          then setHandicapsRoomAlone (head studentsOwnRoom) (head rooms') exams'
          else error "more than one students needs room for its own"
      return ()

setHandicapRoomOnAllExams :: AvailableRoom -> [Exam]
                          -> Writer [AddRoomToExam] ()
setHandicapRoomOnAllExams room  = mapM_ (setHandicapRoomOnExam room)
  where
    setHandicapRoomOnExam :: AvailableRoom -> Exam -> Writer [AddRoomToExam] ()
    setHandicapRoomOnExam availableRoom exam = do
      let studentsWithHandicaps' =
            filter (maybe False (not . handicapNeedsRoomAlone) . studentHandicap)
                   (registeredStudents exam)
          deltaDuration'  = (duration exam  * maximum
            (map (handicapDeltaDurationPercent . fromJust . studentHandicap)
                 studentsWithHandicaps')) `div` 100
          roomName = availableRoomName availableRoom
      when (withHandicaps exam && not (null studentsWithHandicaps')) $
        tell [AddRoomToExam (anCode exam)
                            roomName
                            (map studentMtknr studentsWithHandicaps')
                            (Just deltaDuration')]
      return ()

setHandicapsRoomAlone :: StudentWithRegs -> AvailableRoom -> [Exam] -> Writer [AddRoomToExam] ()
setHandicapsRoomAlone stud availableRoom exams' = do
  let [exam] = filter (any handicapNeedsRoomAlone
                      . map (fromJust . studentHandicap)
                      . handicapStudents) exams'
      roomName = availableRoomName availableRoom
      deltaDuration'  = (duration exam  *
        handicapDeltaDurationPercent (fromJust $ studentHandicap stud)) `div` 100
  tell [AddRoomToExam (anCode exam) roomName [studentMtknr stud] (Just deltaDuration')]
  return ()
