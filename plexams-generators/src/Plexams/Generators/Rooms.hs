module Plexams.Generators.Rooms
  ( generateRooms
  )
where

import           Control.Monad.Writer
import           Data.List                      ( (\\)
                                                , partition
                                                , sortOn
                                                , splitAt
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                , mapMaybe
                                                )
import           Data.Ord                       ( Down(Down) )
import           GHC.Exts                       ( sortWith )
import           Plexams.Types

-- TODO: complete rewrite, set all rooms in one slot in one function including NTA
generateRooms :: Plan -> [AddRoomToExam]
generateRooms plan =
  let availableRooms' = M.toList
        $ mkAvailableRooms plan (availableRooms $ semesterConfig plan)
      slots' = M.toList $ M.map (M.elems . examsInSlot) $ slots plan
      roomsAndSlots =
        [ (s, (rooms', filter plannedByMe exams'))
        | (s , rooms') <- availableRooms'
        , (s', exams') <- slots'
        , s == s'
        ]
      slotsWithRooms = foldl
        (\roomsAndSlots' (slot'@(_, s), roomAndSlot) -> if null roomsAndSlots'
          then [(slot', setRoomsOnSlot [] roomAndSlot)]
          else if s == 0 -- maxSlotIndex plan
            then (slot', setRoomsOnSlot [] roomAndSlot) : roomsAndSlots'
            else
              ( slot'
                , setRoomsOnSlot (snd $ snd $ head roomsAndSlots') roomAndSlot
                )
                : roomsAndSlots'
        )
        []
        roomsAndSlots
  in  sortWith addRoomAnCode $ concatMap (fst . snd) slotsWithRooms

setRoomsOnSlot
  :: [AvailableRoom]
  -> (([AvailableRoom], [AvailableRoom], [AvailableRoom]), [Exam])
  -> ([AddRoomToExam], [AvailableRoom])
setRoomsOnSlot usedInSlotBefore ((n, nR, h), e) =
  (\(r', a) -> (a, r')) $ runWriter $ do
    (e', doNotUseInNextSlot) <- setNormalRoomsOnSlot usedInSlotBefore [] n nR e
    usedInThisSlot           <- setHandicapRoomsOnSlot usedInSlotBefore h e'
    return $ doNotUseInNextSlot ++ usedInThisSlot

handicapInNormalRoom :: Exam -> Bool
handicapInNormalRoom exam =
  let noOfStudentsInSameRoom =
        length
          $ filter (not . handicapNeedsRoomAlone)
          $ mapMaybe studentHandicap
          $ handicapStudents exam
      noOfRegisteredStudents = length $ registeredStudents exam
  in  noOfStudentsInSameRoom
      >  0
      && noOfRegisteredStudents
      /= noOfStudentsInSameRoom
      && noOfRegisteredStudents
      <= 22

seatsMissing' :: Exam -> Integer
seatsMissing' exam =
  seatsMissing exam
    - (if null (handicapStudents exam) || handicapInNormalRoom exam
        then 0
        else toInteger (length (handicapStudents exam))
      )

moreExamsInSameRoom :: Exam -> ([Exam], [Exam]) -> ([Exam], [Exam])
moreExamsInSameRoom exam exams@([], examsOtherRooms') =
  let sumExam = length $ registeredStudents exam
      (shareRoomExams, doNotShareRoomExams) =
        partition shareRoom examsOtherRooms'
      sumNext = length $ registeredStudents $ head shareRoomExams
  in  if not (null shareRoomExams)
           && 0
           <  sumExam
           && sumExam
           <= 20
           && sumExam
           +  sumNext
           <= 20
        then (take 1 shareRoomExams, tail shareRoomExams ++ doNotShareRoomExams)
        else exams -- TODO: muss nicht [] sein!
moreExamsInSameRoom _ exams = exams

setNormalRoomsOnSlot
  :: [AvailableRoom]
  -> [AvailableRoom]
  -> [AvailableRoom]
  -> [AvailableRoom]
  -> [Exam]
  -> Writer [AddRoomToExam] ([Exam], [AvailableRoom])
setNormalRoomsOnSlot _ _ [] [] e =
  error $ "no normal rooms left for slot\n" ++ show e
setNormalRoomsOnSlot _ usedInThisSlot _ _ [] = return ([], usedInThisSlot)
setNormalRoomsOnSlot usedInSlotBefore usedInThisSlot ownRooms'' requestRooms'' exams'
  = do
    let
      (nextExam : sortedExams) = sortOn (Down . seatsMissing') exams'
      ownRooms                 = ownRooms'' \\ usedInSlotBefore
      requestRooms             = requestRooms'' \\ usedInSlotBefore
      seatsMissing'' =
        seatsMissing' nextExam + sum (map seatsMissing' otherExamsInSameRoom)
      seatsMissingSecond =
        if null sortedExams then 0 else seatsMissing' (head sortedExams)
      (otherExamsInSameRoom, otherExams) =
        moreExamsInSameRoom nextExam
          $ partition ((`elem` sameRoom nextExam) . anCode) sortedExams
      otherExamsInSameRoomWithCount = map
        (\e -> (e, toInteger $ length $ registeredStudents e))
        otherExamsInSameRoom
      (room, ownRooms', requestRooms') = if null ownRooms
        then error $ "no own rooms left for slot\n" ++ show nextExam
        else
          let room' = head ownRooms
          in
            if seatsMissing''
               <= availableRoomMaxSeats room'
               || null requestRooms
            then
              (room', tail ownRooms, requestRooms)
            else
              if seatsMissingSecond
                 >  availableRoomMaxSeats room'
                 && length requestRooms
                 >  1
              then
                (last requestRooms, ownRooms, init requestRooms)
              else
                (head requestRooms, ownRooms, tail requestRooms)
    if seatsMissing'' <= 0
      then return (exams', usedInThisSlot)
      else do
        examsWithRoomAndUsedRooms <-
          mapM (`setRoomOnExam` room)
          $ ( nextExam
            , availableRoomMaxSeats room
              - sum (map snd otherExamsInSameRoomWithCount)
            )
          : otherExamsInSameRoomWithCount
        let examsWithRoom = map fst examsWithRoomAndUsedRooms
            usedInThisSlot' =
              concatMap snd examsWithRoomAndUsedRooms ++ usedInThisSlot
        setNormalRoomsOnSlot usedInSlotBefore
                             usedInThisSlot'
                             ownRooms'
                             requestRooms'
          $  examsWithRoom
          ++ otherExams

setRoomOnExam
  :: (Exam, Integer)
  -> AvailableRoom
  -> Writer [AddRoomToExam] (Exam, [AvailableRoom])
setRoomOnExam (exam, maxUsableSeats) availableRoom = do
  let (studsWithoutHandicap, studentsWithHandicaps') =
        partition (isNothing . studentHandicap) $ registeredStudents exam
      (studentsWithHandicapsNotRoomAlone, studentsWithHandicapsRoomAlone) =
        partition (not . handicapNeedsRoomAlone . fromJust . studentHandicap)
                  studentsWithHandicaps'
      handicapInNormalRoom' =
        not (null (handicapStudents exam)) && handicapInNormalRoom exam
      (studsForRoom, restOfStuds) = splitAt
        (fromInteger maxUsableSeats - if handicapInNormalRoom'
          then length studentsWithHandicapsNotRoomAlone
          else 0
        )
        studsWithoutHandicap
      roomName = availableRoomName availableRoom
      deltaDuration' =
        (duration exam * maximum
            (map (handicapDeltaDurationPercent . fromJust . studentHandicap)
                 studentsWithHandicapsNotRoomAlone
            )
          )
          `div` 100
  unless (null studsForRoom) $ tell
    [ AddRoomToExam (anCode exam)
                    roomName
                    (map studentMtknr studsForRoom)
                    Nothing
                    False
                    False
    ]
  when (handicapInNormalRoom' && not (null studentsWithHandicapsNotRoomAlone))
    $ tell
        [ AddRoomToExam (anCode exam)
                        roomName
                        (map studentMtknr studentsWithHandicapsNotRoomAlone)
                        (Just deltaDuration')
                        True
                        False
        ]
  return
    ( exam
      { rooms              =
        Room
            { roomID               = roomName
            , maxSeats             = availableRoomMaxSeats availableRoom
            , deltaDuration        = 0
            , invigilator          = Nothing
            , reserveRoom          = length studsForRoom
              < fromInteger (registrations exam `div` 10)
            , handicapCompensation = False
            , studentsInRoom       = studsForRoom ++ if handicapInNormalRoom'
              then studentsWithHandicapsNotRoomAlone
              else []
            }
          : rooms exam
      , registeredStudents = restOfStuds ++ if handicapInNormalRoom'
                               then studentsWithHandicapsRoomAlone
                               else studentsWithHandicaps'
      }
    , [ availableRoom
      | handicapInNormalRoom' && not (null studentsWithHandicapsNotRoomAlone)
      ]
    )

setHandicapRoomsOnSlot
  :: [AvailableRoom]
  -> [AvailableRoom]
  -> [Exam]
  -> Writer [AddRoomToExam] [AvailableRoom]
setHandicapRoomsOnSlot _ [] _ = error "no handicap rooms left for slot"
setHandicapRoomsOnSlot roomsUsedSlotBefore rooms'' exams'
  | not (any withHandicaps exams') = return []
  | otherwise = do
    let (room : rooms')    = rooms'' \\ roomsUsedSlotBefore
        examsWithHandicaps = filter (not . null . registeredStudents) exams'
        (studentsOwnRoom, otherStudents) =
          partition (handicapNeedsRoomAlone . fromJust . studentHandicap)
            $ filter (isJust . studentHandicap)
            $ concatMap registeredStudents examsWithHandicaps
    roomUsed <-
      if length otherStudents <= fromInteger (availableRoomMaxSeats room)
        then setHandicapRoomOnAllExams room exams'
        else error "too much handicap students, room to small"
    unless (null studentsOwnRoom) $ if length studentsOwnRoom == 1
      then setHandicapsRoomAlone (head studentsOwnRoom)
                                 (if roomUsed then head rooms' else room)
                                 exams'
      else error "more than one students needs room for its own"
    return $ if null studentsOwnRoom && not roomUsed
      then []
      else if not (null studentsOwnRoom) && roomUsed
        then room : take 1 rooms'
        else [room]

setHandicapRoomOnAllExams
  :: AvailableRoom -> [Exam] -> Writer [AddRoomToExam] Bool
setHandicapRoomOnAllExams room = fmap or . mapM (setHandicapRoomOnExam room)
 where
  setHandicapRoomOnExam :: AvailableRoom -> Exam -> Writer [AddRoomToExam] Bool
  setHandicapRoomOnExam availableRoom exam = do
    let studentsWithHandicaps' = filter
          (maybe False (not . handicapNeedsRoomAlone) . studentHandicap)
          (registeredStudents exam)
        deltaDuration' =
          (duration exam * maximum
              (map (handicapDeltaDurationPercent . fromJust . studentHandicap)
                   studentsWithHandicaps'
              )
            )
            `div` 100
        roomName = availableRoomName availableRoom
    if withHandicaps exam && not (null studentsWithHandicaps') && not
         (handicapInNormalRoom exam)
      then do
        tell
          [ AddRoomToExam (anCode exam)
                          roomName
                          (map studentMtknr studentsWithHandicaps')
                          (Just deltaDuration')
                          True
                          False
          ]
        return True
      else return False

setHandicapsRoomAlone
  :: StudentWithRegs -> AvailableRoom -> [Exam] -> Writer [AddRoomToExam] ()
setHandicapsRoomAlone stud availableRoom exams' = do
  let [exam] = filter
        ( any handicapNeedsRoomAlone
        . map (fromJust . studentHandicap)
        . handicapStudents
        )
        exams'
      roomName = availableRoomName availableRoom
      deltaDuration' =
        ( duration exam
          * handicapDeltaDurationPercent (fromJust $ studentHandicap stud)
          )
          `div` 100
  tell
    [ AddRoomToExam (anCode exam)
                    roomName
                    [studentMtknr stud]
                    (Just deltaDuration')
                    True
                    False
    ]
  return ()
