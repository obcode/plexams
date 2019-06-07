{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plexams.Validation.Invigilation
  ( validate
  )
where

import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                , first
                                                , second
                                                )
import           Control.Monad.Writer
import           Data.List                      ( (\\)
                                                , intersect
                                                , nub
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isNothing
                                                , mapMaybe
                                                )
import           Data.Text                      ( append
                                                , pack
                                                )
import           GHC.Exts                       ( groupWith
                                                , sortWith
                                                )

import           TextShow                       ( showt )

import           Plexams.Invigilation
import           Plexams.Query                  ( queryByLecturerID )
import           Plexams.Types

validate :: Plan -> Writer [ValidationRecord] ValidationResult
validate plan = do
  tell [ValidationRecord Info "## Validating Invigilations"]
  invigilatorOk            <- validateInvigilator plan
  handicapsOk              <- validateHandicapsInvigilator plan
  allSlotsHaveReserves     <- validateAllSlotsHaveReserves plan
  -- TODO: Reserve nicht im nächsten Slot, wenn ein (langer) NTA mit im Slot ist
  allRoomsHaveInvigilators <- validateAllRoomsHaveInvigilators plan
  return $ validationResult
    [allSlotsHaveReserves, allRoomsHaveInvigilators, invigilatorOk, handicapsOk]

validateAllSlotsHaveReserves
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateAllSlotsHaveReserves plan = do
  tell [ValidationRecord Info "### Validating all slots have a reserve invigilator"]
  let slotsWithoutReserve =
        filter (isNothing . snd)
          $ map (second reserveInvigilator)
          $ filter (any plannedByMe . M.elems . examsInSlot . snd)
          $ M.toList
          $ slots plan
  forM_ slotsWithoutReserve $ \(s, _) -> tell
    [ ValidationRecord HardConstraintBroken
      $        "- Slot "
      `append` showt s
      `append` ": no reserve defined"
    ]
  return
    $ if null slotsWithoutReserve then EverythingOk else HardConstraintsBroken

validateAllRoomsHaveInvigilators
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateAllRoomsHaveInvigilators plan = do
  tell [ValidationRecord Info "### Validating all rooms have an invigilator"]
  let
    allRoomsWithoutInvigilator =
      filter (isNothing . snd)
        $ concatMap
            (\exam' -> map (\r -> ((anCode exam', roomID r), invigilator r))
                           (rooms exam')
            )
        $ scheduledExams plan
  forM_ allRoomsWithoutInvigilator $ \((ancode, roomID'), _) -> tell
    [ ValidationRecord HardConstraintBroken
      $        "- Exam "
      `append` showt ancode
      `append` ", Room "
      `append` pack roomID'
      `append` ": no invigilator defined"
    ]
  return $ if null allRoomsWithoutInvigilator
    then EverythingOk
    else HardConstraintsBroken

validateInvigilator :: Plan -> Writer [ValidationRecord] ValidationResult
validateInvigilator plan' = do
  tell [ValidationRecord Info "### Validating invigilators constraints"]
  let
    plan   = setSlotsOnExams plan'
    slots' = M.elems $ slots plan
    reserveInvigilatorIds =
      map invigilatorID $ mapMaybe reserveInvigilator slots'
    invigilatorInRoomIds = map invigilatorID $ mapMaybe invigilator $ concatMap
      (concatMap rooms . M.elems . examsInSlot)
      slots'
    invigilatorIds          = M.keys $ invigilators plan
    invigilationsPerPerson' = invigilationsPerPerson plan
    invigilationsOfPerson invigID =
      M.findWithDefault [] invigID invigilationsPerPerson'
  validationResult <$> mapM
    (\invigID -> validateInvigilator' plan
                                      invigID
                                      (invigilationsOfPerson invigID)
                                      (examsThatShareRooms plan)
    )
    (nub (reserveInvigilatorIds ++ invigilatorInRoomIds ++ invigilatorIds))

examsShareRoom :: Exam -> Exam -> Bool
examsShareRoom e1 e2 =
  let roomIDs = map roomID . rooms
  in  not $ null $ roomIDs e1 `intersect` roomIDs e2

examsWithSharedRoom :: [Exam] -> [Exam]
examsWithSharedRoom es =
  nub $ concat [ [e1, e2] | e1 <- es, e2 <- es, e1 /= e2, examsShareRoom e1 e2 ]

mkSharedRoomsMap :: [[Exam]] -> M.Map (Ancode, RoomID) [Exam]
mkSharedRoomsMap = foldr
  (\exams sharedRoomsMap -> foldr
    (\exam sharedRoomsMap' ->
      let
        ac         = anCode exam
        otherExams = exams \\ [exam]
      in
        foldr
            ( (\roomID' sharedRoomsMap'' ->
                let otherExamsWithSameRoom =
                      filter ((roomID' `elem`) . map roomID . rooms) otherExams
                in  if null otherExamsWithSameRoom
                      then sharedRoomsMap'
                      else M.insert (ac, roomID')
                                    otherExamsWithSameRoom
                                    sharedRoomsMap''
              )
            . roomID
            )
            sharedRoomsMap'
          $ rooms exam
    )
    sharedRoomsMap
    exams
  )
  M.empty

examsThatShareRooms :: Plan -> M.Map (Ancode, RoomID) [Exam]
examsThatShareRooms =
  mkSharedRoomsMap
    . map (examsWithSharedRoom . M.elems . examsInSlot)
    . M.elems
    . slots

validateInvigilator'
  :: Plan
  -> PersonID
  -> [Invigilation]
  -> M.Map (Ancode, RoomID) [Exam]
  -> Writer [ValidationRecord] ValidationResult
validateInvigilator' _ _ [] _ = return EverythingOk -- ?
validateInvigilator' plan invigilatorID' invigilations' examsThatShareRoomsMap
  = do
    let maybeInvigilator = M.lookup invigilatorID' $ invigilators plan
    case maybeInvigilator of
      Nothing -> do
        tell
          [ ValidationRecord SoftConstraintBroken
            $        "- InvigilatorID "
            `append` showt invigilatorID'
            `append` " not found in invigilators"
          ]
        return SoftConstraintsBroken
      Just invigilator' -> do
        let invigilationDays = nub $ map invigilationDay invigilations'
        maxThreeDays <- validateMaxThreeDays invigilator' invigilationDays
        daysOk                  <- validateDaysOk invigilator' invigilationDays
        numberOfMinutesOk       <- validateNumberOfMinutesOk invigilator'
        notMoreThanOnceInSlotOk <- validateNotMoreThanOnceInSlotOk
          invigilator'
          invigilations'
        notReserveOrInvigilatorIfExamInSlotOk <-
          validateNotReserveOrInvigilatorIfExamInSlotOk plan
                                                        invigilatorID'
                                                        invigilator'
        allOwnSelfOk <- validateAllOwnSelfOk plan
                                             invigilator'
                                             examsThatShareRoomsMap
        return $ validationResult
          [ maxThreeDays
          , daysOk
          , numberOfMinutesOk
          , notMoreThanOnceInSlotOk
          , notReserveOrInvigilatorIfExamInSlotOk
          , allOwnSelfOk
          ]

validateMaxThreeDays
  :: Invigilator -> [Int] -> Writer [ValidationRecord] ValidationResult
validateMaxThreeDays invigilator' invigilationDays = do
  let maxThreeDays = if length invigilationDays <= 3
        then EverythingOk
        else SoftConstraintsBroken
  unless (maxThreeDays == EverythingOk) $ tell
    [ ValidationRecord SoftConstraintBroken
      $        "- "
      `append` invigilatorName invigilator'
      `append` " invigilations on more than three days: "
      `append` showt invigilationDays
    ]
  return maxThreeDays

validateDaysOk
  :: Invigilator -> [Int] -> Writer [ValidationRecord] ValidationResult
validateDaysOk invigilator' invigilationDays = do
  let daysOk
        | null (invigilationDays \\ invigilatorWantDays invigilator')
        = EverythingOk
        | null
          (  (invigilationDays \\ invigilatorWantDays invigilator')
          \\ invigilatorCanDays invigilator'
          )
        = SoftConstraintsBroken
        | otherwise
        = HardConstraintsBroken
  when (daysOk == HardConstraintsBroken) $ tell
    [ ValidationRecord HardConstraintBroken
      $        "- "
      `append` invigilatorName invigilator'
      `append` " invigilations on wrong days: "
      `append` showt invigilationDays
    ]
  when (daysOk == SoftConstraintsBroken) $ tell
    [ ValidationRecord SoftConstraintBroken
      $        "- "
      `append` invigilatorName invigilator'
      `append` " invigilations on can days: "
      `append` showt invigilationDays
    ]
  return daysOk

validateNumberOfMinutesOk
  :: Invigilator -> Writer [ValidationRecord] ValidationResult
validateNumberOfMinutesOk invigilator' = do
  let minutesLeft = invigilatorMinutesTodo invigilator'
        - invigilatorsMinutesPlanned invigilator'
      numberOfMinutesOk | minutesLeft > -90 = EverythingOk
                        | otherwise         = SoftConstraintsBroken
  unless (numberOfMinutesOk == EverythingOk) $ tell
    [ ValidationRecord SoftConstraintBroken
      $        "- "
      `append` invigilatorName invigilator'
      `append` " has too much invigilations "
      `append` showt minutesLeft
    ]
  return numberOfMinutesOk

validateNotMoreThanOnceInSlotOk
  :: Invigilator -> [Invigilation] -> Writer [ValidationRecord] ValidationResult
validateNotMoreThanOnceInSlotOk invigilator' invigilations' = do
  let
    notMoreThanOnceInSlot =
      filter ((> 1) . length)
        $ map
            ( nub
            . map
                (\i ->
                  (invigilationDay i, invigilationSlot i, invigilationRoom i)
                )
            )
        $ groupWith (invigilationDay &&& invigilationSlot) invigilations'
    notMoreThanOnceInSlotOk | null notMoreThanOnceInSlot = EverythingOk
                            | otherwise                  = HardConstraintsBroken
  unless (notMoreThanOnceInSlotOk == EverythingOk) $ tell
    [ ValidationRecord HardConstraintBroken
      $        "- "
      `append` invigilatorName invigilator'
      `append` " has been scheduled more then once in slot  "
      `append` showt notMoreThanOnceInSlot
    ]
  return notMoreThanOnceInSlotOk

validateNotReserveOrInvigilatorIfExamInSlotOk
  :: Plan
  -> PersonID
  -> Invigilator
  -> Writer [ValidationRecord] ValidationResult
validateNotReserveOrInvigilatorIfExamInSlotOk plan invigilatorID' invigilator'
  = do
    let
      ownReserveSlots =
        map fst
          $ filter ((== Just True) . snd)
          $ map
              (second
                (fmap ((== invigilatorID') . invigilatorID) . reserveInvigilator
                )
              )
          $ M.toList
          $ slots plan
      ownExamSlots =
        mapMaybe slot
          $ filter ((== invigilatorID') . personID . lecturer)
          $ scheduledExams plan
      ownInvigilatorSlots =
        mapMaybe slot
          $ filter
              (\e ->
                not
                  (  personID (lecturer e)
                  == invigilatorID'
                  && length (rooms e)
                  == 1
                  )
              )
          $ filter
              ( elem (Just invigilatorID')
              . map (fmap invigilatorID . invigilator)
              . rooms
              )
          $ scheduledExams plan
      notReserveOrInvigilatorIfExamInSlotOk =
        if null (ownExamSlots `intersect` ownReserveSlots)
             &&
-- null (ownExamSlots `intersect` ownInvigilatorSlots) &&
                null (ownReserveSlots `intersect` ownInvigilatorSlots)
          then EverythingOk
          else HardConstraintsBroken
    unless (notReserveOrInvigilatorIfExamInSlotOk == EverythingOk) $ tell
      [ ValidationRecord HardConstraintBroken
        $        "- "
        `append` invigilatorName invigilator'
        `append` " has been scheduled as a reserve and invigilator"
        `append` " during the same time, or reserve during own exams"
        `append` " (exam, invig, reserve)"
        `append` showt (ownExamSlots, ownInvigilatorSlots, ownReserveSlots)
      ]
    return notReserveOrInvigilatorIfExamInSlotOk

validateAllOwnSelfOk
  :: Plan
  -> Invigilator
  -> M.Map (Ancode, RoomID) [Exam]
  -> Writer [ValidationRecord] ValidationResult
validateAllOwnSelfOk plan invigilator' examsThatShareRoomsMap = do
  let
    invigID = invigilatorID invigilator'
    roomsOfOwnExamsWithOneRoom :: [[[(Room, (Maybe (Int, Int), Exam))]]]
    roomsOfOwnExamsWithOneRoom =
      filter ((== 1) . length)
        $ map
            (\exam ->
              groupWith (roomID . fst) $ map (, (slot exam, exam)) $ rooms exam
            )
        $ queryByLecturerID invigID plan
    roomsOfOwnExamsWithOneRoomOtherInvig = concat $ filter
      (\rs ->
        let maybeInvig = invigilator $ fst $ head $ map head rs
        in  case maybeInvig of
              Nothing     -> False
              Just invig' -> invigilatorID invig' /= invigID
      )
      roomsOfOwnExamsWithOneRoom
    roomsOfOwnExamsWithOneRoomOtherInvigWithoutExcludedDays
      :: [(Room, ((Int, Int), Exam))]
    roomsOfOwnExamsWithOneRoomOtherInvigWithoutExcludedDays =
      filter
          ((`notElem` invigilatorExcludedDays invigilator') . fst . fst . snd)
        $ map (second (first fromJust))
        $ concat roomsOfOwnExamsWithOneRoomOtherInvig
    roomIsSharedAndOtherInvigsOwnExam :: (Room, ((Int, Int), Exam)) -> Bool
    roomIsSharedAndOtherInvigsOwnExam (r, (_, e)) =
      case M.lookup (anCode e, roomID r) examsThatShareRoomsMap of
        Nothing -> False
        Just [] -> False
        Just es ->
          any (\e' -> length (rooms e') == 1 && lecturerIsInvigilator e')
            $ filter ((/= lecturer e) . lecturer) es
         where
          lecturerIsInvigilator exam = case invigilator $ head $ rooms exam of
            Nothing    -> False
            Just invig -> personID (lecturer exam) == invigilatorID invig
    roomIsSharedAndOtherExamOfSameLecturerHasMoreRooms (r, (_, e)) =
      case M.lookup (anCode e, roomID r) examsThatShareRoomsMap of
        Nothing -> False
        Just [] -> False
        Just es -> any (\e' -> length (rooms e') > 1)
          $ filter ((== lecturer e) . lecturer) es
    removeShareRooms =
      filter (not . roomIsSharedAndOtherExamOfSameLecturerHasMoreRooms) $ filter
        (not . roomIsSharedAndOtherInvigsOwnExam)
        roomsOfOwnExamsWithOneRoomOtherInvigWithoutExcludedDays
    allOwnSelfOk =
      if null removeShareRooms then EverythingOk else HardConstraintsBroken
    -- Special case: lecturer has two different exams in two different rooms
    twoExamsInTwoRooms =
      any ((> 1) . length)
        $ map (nub . map (concatMap roomID . rooms))
        $ filter ((> 1) . length)
        $ map
            ( filter ((== invigilatorID invigilator') . personID . lecturer)
            . M.elems
            . examsInSlot
            )
        $ M.elems
        $ slots plan
  unless (allOwnSelfOk == EverythingOk || twoExamsInTwoRooms) $ tell
    [ ValidationRecord HardConstraintBroken
      $        "- "
      `append` invigilatorName invigilator'
      `append` " is not invigilator of his own exam"
      `append` showt (map (roomID *** second anCode) removeShareRooms)
    ]
  return allOwnSelfOk

validateHandicapsInvigilator
  :: Plan -> Writer [ValidationRecord] ValidationResult
validateHandicapsInvigilator plan = do
  tell [ValidationRecord Info "### Validating handicap invigilators constraints"]
  let days' =
        map slotAndNextSlot $ groupWith (fst . fst) $ M.toList $ slots plan
      slotAndNextSlot day' =
        let slots' = sortWith (snd . fst) day' in zip slots' (tail slots')
  validationResult
    <$> mapM (fmap validationResult . mapM validateHandicapsInvigilator') days'
  -- a invigilator in a handicaps room should not be involved in the
  -- following slot

validateHandicapsInvigilator'
  :: (((DayIndex, SlotIndex), Slot), ((DayIndex, SlotIndex), Slot))
  -> Writer [ValidationRecord] ValidationResult
validateHandicapsInvigilator' ((si@(d1, s1), slot'), ((d2, s2), nextSlot)) = do
  when (d1 /= d2) $ tell
    [ ValidationRecord HardConstraintBroken
        ">>> This should not happen. Validating handicaps on different days"
    ]
  when (s1 + 1 /= s2) $ tell
    [ ValidationRecord HardConstraintBroken
        ">>> This should not happen. Validating handicaps on non-following slots"
    ]
  let
    handicapInvigilators = nub $ catMaybes $ concatMap
      ( (map (fmap invigilatorID . invigilator . snd) . filter
          (\(d, r) -> handicapCompensation r && d + deltaDuration r > 100)
        )
      . (\e -> map (duration e, ) $ rooms e)
      )
      (M.elems $ examsInSlot slot')

    personsInvolvedInNextSlot = nub $ catMaybes
      ((invigilatorID <$> reserveInvigilator nextSlot) : concatMap
        (\e -> Just (personID $ lecturer e)
          : map (fmap invigilatorID . invigilator) (rooms e)
        )
        (M.elems $ examsInSlot nextSlot)
      )
    handicapInvigilatorInvolvedInNextSlot =
      any (`elem` personsInvolvedInNextSlot) handicapInvigilators
  when handicapInvigilatorInvolvedInNextSlot $ tell
    [ ValidationRecord HardConstraintBroken
      $        "- Handicap invigilator (slot "
      `append` showt si
      `append` ") is involved in next slot"
    ]
  return $ if handicapInvigilatorInvolvedInNextSlot
    then HardConstraintsBroken
    else EverythingOk
