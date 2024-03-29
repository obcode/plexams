module Plexams.PlanManip.Invigilator
  ( applyAddInvigilatorsToPlan
  , removeInvigilatorFromExamOrSlot
  , addInvigilatorToExamOrSlot
  ) where

import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Plexams.Invigilation (minutesForReserve)
import Plexams.Types

-- TODO: add info about not corrent AddInvigilatorToRoomOrSlot values
-- TODO: add check: Add invigilators NOT before the room allocation is frozen
applyAddInvigilatorsToPlan :: Plan -> [AddInvigilatorToRoomOrSlot] -> Plan
applyAddInvigilatorsToPlan = foldr applyAddInvigilators
  where
    applyAddInvigilators (AddInvigilatorToRoomOrSlot personID' slot' maybeRoom) =
      addInvigilatorToExamOrSlot personID' slot' maybeRoom

data WhatToDo
  = Add
  | Remove
  deriving (Eq)

removeInvigilatorFromExamOrSlot ::
     Integer -> (DayIndex, SlotIndex) -> Maybe String -> Plan -> Plan
removeInvigilatorFromExamOrSlot personID' slotKey maybeRoom plan =
  fromMaybe plan $
  addOrRemoveInvigilatorFromExamOrSlot Remove personID' slotKey maybeRoom plan

addInvigilatorToExamOrSlot ::
     Integer -> (DayIndex, SlotIndex) -> Maybe String -> Plan -> Plan
addInvigilatorToExamOrSlot personID' slotKey maybeRoom plan =
  fromMaybe plan $
  addOrRemoveInvigilatorFromExamOrSlot Add personID' slotKey maybeRoom plan

addOrRemoveInvigilatorFromExamOrSlot ::
     WhatToDo
  -> Integer
  -> (DayIndex, SlotIndex)
  -> Maybe String
  -> Plan
  -> Maybe Plan
addOrRemoveInvigilatorFromExamOrSlot whatToDo personID' slotKey maybeRoom plan =
  case maybeRoom of
    Nothing -> addOrRemoveInvigilatorSlot whatToDo personID' slotKey plan
    Just roomID' ->
      addOrRemoveInvigilatorExam whatToDo personID' slotKey roomID' plan

addOrRemoveInvigilatorSlot ::
     WhatToDo -> Integer -> (DayIndex, SlotIndex) -> Plan -> Maybe Plan
addOrRemoveInvigilatorSlot whatToDo personID' slotKey plan = do
  invigilator' <- M.lookup personID' $ invigilators plan
  slot' <- M.lookup slotKey $ slots plan
  let maybeReserveInvigilator' = reserveInvigilator slot'
  case whatToDo of
    Remove ->
      case maybeReserveInvigilator' of
        Just reserveInvigilator'
          | invigilatorID invigilator' == invigilatorID reserveInvigilator' ->
            Just $
            plan
            { slots =
                M.insert slotKey (slot' {reserveInvigilator = Nothing}) $
                slots plan
            , invigilators =
                M.insert
                  (invigilatorID invigilator')
                  (invigilator'
                   { invigilatorsMinutesPlanned =
                       invigilatorsMinutesPlanned invigilator' -
                       minutesForReserve
                   , invigilatorInvigilationDays =
                       invigilatorInvigilationDays invigilator' \\ [fst slotKey]
                   }) $
                invigilators plan
            }
        _ -> Nothing
    Add ->
      case maybeReserveInvigilator' of
        Just reserveInvigilator'
          | invigilatorID invigilator' /= invigilatorID reserveInvigilator' -> do
            plan' <-
              addOrRemoveInvigilatorSlot
                Remove
                (invigilatorID reserveInvigilator')
                slotKey
                plan
            addOrRemoveInvigilatorSlot Add personID' slotKey plan'
        Just _ -> Nothing
        Nothing ->
          Just $
          plan
          { slots =
              M.insert slotKey (slot' {reserveInvigilator = Just invigilator'}) $
              slots plan
          , invigilators =
              M.insert
                (invigilatorID invigilator')
                (invigilator'
                 { invigilatorsMinutesPlanned =
                     invigilatorsMinutesPlanned invigilator' + minutesForReserve
                 , invigilatorInvigilationDays =
                     fst slotKey : invigilatorInvigilationDays invigilator'
                 }) $
              invigilators plan
          }

addOrRemoveInvigilatorExam ::
     WhatToDo
  -> Integer
  -> (DayIndex, SlotIndex)
  -> String
  -> Plan
  -> Maybe Plan
addOrRemoveInvigilatorExam whatToDo personID' slotKey roomID' plan = do
  invigilator' <- M.lookup personID' $ invigilators plan
  slot' <- M.lookup slotKey $ slots plan
  let (examsWithRoomID, examsWithoutRoomID) =
        M.partition (elem roomID' . map roomID . rooms) $ examsInSlot slot'
      maximumDurationInRoom =
        if M.null examsWithRoomID
          then 0
          else maximum
                 [ duration e + deltaDuration r
                 | e <- M.elems examsWithRoomID
                 , r <- rooms e
                 , roomID r == roomID'
                 ]
      invigilatorInRoom =
        [ invigilator r
        | e <- M.elems examsWithRoomID
        , r <- rooms e
        , roomID r == roomID'
        ]
  case whatToDo of
    Remove ->
      case invigilatorInRoom of
        (Just invigilatorInRoom':_)
          | invigilatorID invigilator' == invigilatorID invigilatorInRoom' ->
            Just $
            plan
            { slots =
                M.insert
                  slotKey
                  (slot'
                   { examsInSlot =
                       examsWithoutRoomID `M.union`
                       M.map
                         (\e ->
                            e
                            { rooms =
                                map
                                  (\r ->
                                     if roomID r == roomID'
                                       then r {invigilator = Nothing}
                                       else r) $
                                rooms e
                            })
                         examsWithRoomID
                   }) $
                slots plan
            , invigilators =
                M.insert
                  (invigilatorID invigilator')
                  (invigilator'
                   { invigilatorsMinutesPlanned =
                       invigilatorsMinutesPlanned invigilator' -
                       maximumDurationInRoom
                   , invigilatorInvigilationDays =
                       invigilatorInvigilationDays invigilator' \\ [fst slotKey]
                   }) $
                invigilators plan
            }
        _ -> Nothing
    Add ->
      case invigilatorInRoom of
        (Just invigilatorInRoom':_)
          | invigilatorID invigilator' /= invigilatorID invigilatorInRoom' -> do
            plan' <-
              addOrRemoveInvigilatorExam
                Remove
                (invigilatorID invigilatorInRoom')
                slotKey
                roomID'
                plan
            addOrRemoveInvigilatorExam Add personID' slotKey roomID' plan'
        (Nothing:_) ->
          Just $
          plan
          { slots =
              M.insert
                slotKey
                (slot'
                 { examsInSlot =
                     examsWithoutRoomID `M.union`
                     M.map
                       (\e ->
                          e
                          { rooms =
                              map
                                (\r ->
                                   if roomID r == roomID'
                                     then r {invigilator = Just invigilator'}
                                     else r) $
                              rooms e
                          })
                       examsWithRoomID
                 }) $
              slots plan
          , invigilators =
              M.insert
                (invigilatorID invigilator')
                (invigilator'
                 { invigilatorsMinutesPlanned =
                     invigilatorsMinutesPlanned invigilator' +
                     maximumDurationInRoom
                 , invigilatorInvigilationDays =
                     fst slotKey : invigilatorInvigilationDays invigilator'
                 }) $
              invigilators plan
          }
        _ -> Nothing
 -- = undefined
-- f =
--   let invigilator' = M.lookup personID' $ invigilators plan
--       maybeSlot = M.lookup slotKey $ slots plan
--       addInvigilator invigilator'' room exam =
--         exam {rooms = map (addInvigilator' invigilator'' room) $ rooms exam}
--       addInvigilator' invigilator'' roomName' room' =
--         if roomID room' == roomName'
--           then room' {invigilator = invigilator''}
--           else room'
--       maxMinutesInRoom room' =
--         fromJust $
--         fmap
--           ((\l ->
--               if null l
--                 then 0
--                 else maximum l) .
--            concatMap
--              (\e ->
--                 [duration e + deltaDuration r | r <- rooms e, roomID r == room']) .
--            M.elems . examsInSlot) $
--         M.lookup slotKey $ slots plan
--   in plan
--      { slots =
--          M.alter
--            (fmap $ \slot' ->
--               case maybeRoom of
--                 Nothing -> slot' {reserveInvigilator = invigilator'}
--                 Just room ->
--                   slot'
--                   { examsInSlot =
--                       M.map (addInvigilator invigilator' room) $
--                       examsInSlot slot'
--                   })
--            slotKey $
--          slots plan
--      , invigilators =
--          M.alter
--            (fmap $ \invig ->
--               invig
--               { invigilatorsMinutesPlanned =
--                   invigilatorsMinutesPlanned invig +
--                   case maybeRoom of
--                     Nothing -> minutesForReserve
--                     Just room' -> maxMinutesInRoom room'
--               })
--            personID' $
--          invigilators plan
--      }
