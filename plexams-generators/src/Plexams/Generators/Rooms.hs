module Plexams.Generators.Rooms
  ( generateRooms
  ) where

import Control.Monad.Writer
import Data.List ((\\), partition, sortBy, splitAt)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Ord (Down(Down), comparing)
import GHC.Exts (sortWith)
import Plexams.Types

generateRooms :: Plan -> [AddRoomToExam]
generateRooms plan =
  let availableRooms' =
        M.toList $ mkAvailableRooms plan (availableRooms $ semesterConfig plan)
      slots' = M.toList $ M.map (M.elems . examsInSlot) $ slots plan
      roomsAndSlots =
        [ (s, (rooms', filter plannedByMe exams'))
        | (s, rooms') <- availableRooms'
        , (s', exams') <- slots'
        , s == s'
        ]
      slotsWithRooms =
        foldr
          (\(slot'@(_, s), roomAndSlot) roomsAndSlots' ->
             if null roomsAndSlots'
               then [(slot', setRoomsOnSlot [] roomAndSlot)]
               else if s == 0
                      then (slot', setRoomsOnSlot [] roomAndSlot) :
                           roomsAndSlots'
                      else ( slot'
                           , setRoomsOnSlot
                               (snd $ snd $ head roomsAndSlots')
                               roomAndSlot) :
                           roomsAndSlots')
          []
          roomsAndSlots
  in sortWith addRoomAnCode $ concatMap (fst . snd) slotsWithRooms

setRoomsOnSlot ::
     [AvailableRoom]
  -> (([AvailableRoom], [AvailableRoom]), [Exam])
  -> ([AddRoomToExam], [AvailableRoom])
setRoomsOnSlot hUsed ((n, h), e) =
  (\(r', a) -> (a, r')) $
  runWriter $ do
    _ <- setNormalRoomsOnSlot n e
    setHandicapRoomsOnSlot hUsed h e

seatsMissing' :: Exam -> Integer
seatsMissing' exam =
  seatsMissing exam - toInteger (length (handicapStudents exam))

setNormalRoomsOnSlot ::
     [AvailableRoom] -> [Exam] -> Writer [AddRoomToExam] [Exam]
setNormalRoomsOnSlot [] _ = error "no normal rooms left for slot"
setNormalRoomsOnSlot rooms' exams' = do
  let exams'' = sortBy (comparing (Down . seatsMissing')) exams'
  case exams'' of
    [] -> return exams'
    (nextExam:sortedExams) ->
      let seatsMissing'' =
            seatsMissing' nextExam +
            sum (map seatsMissing' otherExamsInSameRoom)
          (otherExamsInSameRoom, otherExams) =
            partition ((`elem` sameRoom nextExam) . anCode) sortedExams
          nextOwnRoomAtFront = dropWhile availableRoomNeedsRequest rooms'
          (room:rooms'') =
            if null nextOwnRoomAtFront
              then rooms'
              else let room' = head nextOwnRoomAtFront
                   in if seatsMissing'' <= availableRoomMaxSeats room'
                        then nextOwnRoomAtFront
                        else rooms'
      in if seatsMissing'' <= 0
           then return otherExams
           else do
             examsWithRoom <-
               mapM (`setRoomOnExam` room) $ nextExam : otherExamsInSameRoom
             setNormalRoomsOnSlot rooms'' $ examsWithRoom ++ otherExams

setRoomOnExam :: Exam -> AvailableRoom -> Writer [AddRoomToExam] Exam
setRoomOnExam exam availableRoom = do
  let (studsWithoutHandicap, studsWithHandicap) =
        partition (isNothing . studentHandicap) $ registeredStudents exam
      (studsForRoom, restOfStuds) =
        splitAt
          (fromInteger $ availableRoomMaxSeats availableRoom)
          studsWithoutHandicap
      roomName = availableRoomName availableRoom
  tell
    [ AddRoomToExam
        (anCode exam)
        roomName
        (map studentMtknr studsForRoom)
        Nothing
    ]
  return
    exam
    { rooms =
        Room
        { roomID = roomName
        , maxSeats = availableRoomMaxSeats availableRoom
        , deltaDuration = 0
        , invigilator = Nothing
        , reserveRoom =
            length studsForRoom < fromInteger (registrations exam `div` 10)
        , handicapCompensation = False
        , studentsInRoom = studsForRoom
        } :
        rooms exam
    , registeredStudents = restOfStuds ++ studsWithHandicap
    }

setHandicapRoomsOnSlot ::
     [AvailableRoom]
  -> [AvailableRoom]
  -> [Exam]
  -> Writer [AddRoomToExam] [AvailableRoom]
setHandicapRoomsOnSlot _ [] _ = error "no handicap rooms left for slot"
setHandicapRoomsOnSlot roomsUsedSlotBefore rooms'' exams'
  | not (any withHandicaps exams') = return []
  | otherwise = do
    let (room:rooms') = rooms'' \\ roomsUsedSlotBefore
        examsWithHandicaps = filter withHandicaps exams'
        (studentsOwnRoom, otherStudents) =
          partition (handicapNeedsRoomAlone . fromJust . studentHandicap) $
          concatMap handicapStudents examsWithHandicaps
    if length otherStudents <= fromInteger (availableRoomMaxSeats room)
      then setHandicapRoomOnAllExams room exams'
      else error "too much handicap students, room to small"
    unless (null studentsOwnRoom) $
      if length studentsOwnRoom == 1
        then setHandicapsRoomAlone
               (head studentsOwnRoom)
               (if null otherStudents
                  then room
                  else head rooms')
               exams'
        else error "more than one students needs room for its own"
    return $
      room :
      (if not (null studentsOwnRoom) && not (null otherStudents)
         then take 1 rooms'
         else [])

setHandicapRoomOnAllExams ::
     AvailableRoom -> [Exam] -> Writer [AddRoomToExam] ()
setHandicapRoomOnAllExams room = mapM_ (setHandicapRoomOnExam room)
  where
    setHandicapRoomOnExam :: AvailableRoom -> Exam -> Writer [AddRoomToExam] ()
    setHandicapRoomOnExam availableRoom exam = do
      let studentsWithHandicaps' =
            filter
              (maybe False (not . handicapNeedsRoomAlone) . studentHandicap)
              (registeredStudents exam)
          deltaDuration' =
            (duration exam *
             maximum
               (map
                  (handicapDeltaDurationPercent . fromJust . studentHandicap)
                  studentsWithHandicaps')) `div`
            100
          roomName = availableRoomName availableRoom
      when (withHandicaps exam && not (null studentsWithHandicaps')) $
        tell
          [ AddRoomToExam
              (anCode exam)
              roomName
              (map studentMtknr studentsWithHandicaps')
              (Just deltaDuration')
          ]
      return ()

setHandicapsRoomAlone ::
     StudentWithRegs -> AvailableRoom -> [Exam] -> Writer [AddRoomToExam] ()
setHandicapsRoomAlone stud availableRoom exams' = do
  let [exam] =
        filter
          (any handicapNeedsRoomAlone .
           map (fromJust . studentHandicap) . handicapStudents)
          exams'
      roomName = availableRoomName availableRoom
      deltaDuration' =
        (duration exam *
         handicapDeltaDurationPercent (fromJust $ studentHandicap stud)) `div`
        100
  tell
    [ AddRoomToExam
        (anCode exam)
        roomName
        [studentMtknr stud]
        (Just deltaDuration')
    ]
  return ()
