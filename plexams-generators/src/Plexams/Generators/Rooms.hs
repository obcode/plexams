module Plexams.Generators.Rooms
  ( generateRooms
  ) where

import Control.Monad.Writer
import Data.List ((\\), partition, sortBy, splitAt)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
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
               else if s == maxSlotIndex plan
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
  -> (([AvailableRoom], [AvailableRoom], [AvailableRoom]), [Exam])
  -> ([AddRoomToExam], [AvailableRoom])
setRoomsOnSlot usedInSlotBefore ((n, nR, h), e) =
  (\(r', a) -> (a, r')) $
  runWriter $ do
    (_, doNotUseInNextSlot) <- setNormalRoomsOnSlot usedInSlotBefore [] n nR e
    usedInThisSlot <- setHandicapRoomsOnSlot usedInSlotBefore h e
    return $ doNotUseInNextSlot ++ usedInThisSlot

handicapInNormalRoom :: Exam -> Bool
handicapInNormalRoom exam =
  registeredStudentsCount exam <= 22 &&
  any
    (not . handicapNeedsRoomAlone)
    (mapMaybe studentHandicap $ handicapStudents exam)

seatsMissing' :: Exam -> Integer
seatsMissing' exam =
  seatsMissing exam -
  (if null (handicapStudents exam) || handicapInNormalRoom exam
     then 0
     else toInteger (length (handicapStudents exam)))

setNormalRoomsOnSlot ::
     [AvailableRoom]
  -> [AvailableRoom]
  -> [AvailableRoom]
  -> [AvailableRoom]
  -> [Exam]
  -> Writer [AddRoomToExam] ([Exam], [AvailableRoom])
setNormalRoomsOnSlot _ _ [] [] e =
  error $ "no normal rooms left for slot\n" ++ show e
setNormalRoomsOnSlot _ usedInThisSlot _ _ [] = return ([], usedInThisSlot)
setNormalRoomsOnSlot usedInSlotBefore usedInThisSlot ownRooms'' requestRooms'' exams' = do
  let (nextExam:sortedExams) = sortBy (comparing (Down . seatsMissing')) exams'
      ownRooms = ownRooms'' \\ usedInSlotBefore
      requestRooms = requestRooms'' \\ usedInSlotBefore
      seatsMissing'' =
        seatsMissing' nextExam + sum (map seatsMissing' otherExamsInSameRoom)
      (otherExamsInSameRoom, otherExams) =
        partition ((`elem` sameRoom nextExam) . anCode) sortedExams
      (room, ownRooms', requestRooms') =
        if null ownRooms
          then error $ "no own rooms left for slot\n" ++ show nextExam
          else let room' = head ownRooms
               in if seatsMissing'' <= availableRoomMaxSeats room' ||
                     null requestRooms
                    then (room', tail ownRooms, requestRooms)
                    else (head requestRooms, ownRooms, tail requestRooms)
      usedInThisSlot' = usedInThisSlot
  if seatsMissing'' <= 0
    then return (otherExams, usedInThisSlot')
    else do
      examsWithRoom <-
        mapM (`setRoomOnExam` room) $ nextExam : otherExamsInSameRoom
      setNormalRoomsOnSlot
        usedInSlotBefore
        usedInThisSlot'
        ownRooms'
        requestRooms' $
        examsWithRoom ++ otherExams

setRoomOnExam :: Exam -> AvailableRoom -> Writer [AddRoomToExam] Exam
setRoomOnExam exam availableRoom = do
  let (studsWithoutHandicap, studentsWithHandicaps') =
        partition (isNothing . studentHandicap) $ registeredStudents exam
      studentsWithHandicapsNotRoomAlone =
        filter
          (not . handicapNeedsRoomAlone . fromJust . studentHandicap)
          studentsWithHandicaps'
      handicapInNormalRoom' =
        not (null (handicapStudents exam)) && handicapInNormalRoom exam
      (studsForRoom, restOfStuds) =
        splitAt
          (fromInteger (availableRoomMaxSeats availableRoom) -
           if handicapInNormalRoom'
             then length studentsWithHandicapsNotRoomAlone
             else 0)
          studsWithoutHandicap
      roomName = availableRoomName availableRoom
      deltaDuration' =
        (duration exam *
         maximum
           (map
              (handicapDeltaDurationPercent . fromJust . studentHandicap)
              studentsWithHandicapsNotRoomAlone)) `div`
        100
  tell
    [ AddRoomToExam
        (anCode exam)
        roomName
        (map studentMtknr studsForRoom)
        Nothing
        False
    ]
  when handicapInNormalRoom' $
    tell
      [ AddRoomToExam
          (anCode exam)
          roomName
          (map studentMtknr studentsWithHandicapsNotRoomAlone)
          (Just deltaDuration')
          True
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
    , registeredStudents =
        restOfStuds ++
        if handicapInNormalRoom'
          then []
          else studentsWithHandicapsNotRoomAlone
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
        examsWithHandicaps = filter (not . null . registeredStudents) exams'
        (studentsOwnRoom, otherStudents) =
          partition (handicapNeedsRoomAlone . fromJust . studentHandicap) $
          filter (isJust . studentHandicap) $
          concatMap registeredStudents examsWithHandicaps
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
      when
        (withHandicaps exam &&
         not (null studentsWithHandicaps') && not (handicapInNormalRoom exam)) $
        tell
          [ AddRoomToExam
              (anCode exam)
              roomName
              (map studentMtknr studentsWithHandicaps')
              (Just deltaDuration')
              True
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
        True
    ]
  return ()
