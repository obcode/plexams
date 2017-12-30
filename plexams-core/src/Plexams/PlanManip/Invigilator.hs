module Plexams.PlanManip.Invigilator
  ( applyAddInvigilatorsToPlan
  , removeInvigilatorFromExamOrSlot
  , addInvigilatorToExamOrSlot
  ) where

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
addOrRemoveInvigilatorFromExamOrSlot whatToDo personID' slotKey maybeRoom plan = do
  invigilator' <- M.lookup personID' $ invigilators plan
  slot' <- M.lookup slotKey $ slots plan
  case maybeRoom of
    Nothing ->
      addOrRemoveInvigilatorSlot whatToDo invigilator' (slotKey, slot') plan
    Just roomID' ->
      addOrRemoveInvigilatorExam
        whatToDo
        invigilator'
        (slotKey, slot')
        roomID'
        plan

addOrRemoveInvigilatorSlot ::
     WhatToDo
  -> Invigilator
  -> ((DayIndex, SlotIndex), Slot)
  -> Plan
  -> Maybe Plan
addOrRemoveInvigilatorSlot whatToDo invigilator' (slotKey, slot') plan =
  let maybeReserveInvigilator' = reserveInvigilator slot'
  in case whatToDo of
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
                      }) $
                   invigilators plan
               }
           _ -> Nothing
       Add ->
         case maybeReserveInvigilator' of
           Just reserveInvigilator'
             | invigilatorID invigilator' == invigilatorID reserveInvigilator' ->
               Nothing
           Just reserveInvigilator' -> do
             plan' <-
               addOrRemoveInvigilatorSlot
                 Remove
                 reserveInvigilator'
                 (slotKey, slot')
                 plan
             addOrRemoveInvigilatorSlot
               whatToDo
               invigilator'
               (slotKey, slot')
               plan'
           Nothing ->
             Just $
             plan
             { slots =
                 M.insert
                   slotKey
                   (slot' {reserveInvigilator = Just invigilator'}) $
                 slots plan
             , invigilators =
                 M.insert
                   (invigilatorID invigilator')
                   (invigilator'
                    { invigilatorsMinutesPlanned =
                        invigilatorsMinutesPlanned invigilator' +
                        minutesForReserve
                    }) $
                 invigilators plan
             }

addOrRemoveInvigilatorExam ::
     WhatToDo
  -> Invigilator
  -> ((DayIndex, SlotIndex), Slot)
  -> String
  -> Plan
  -> Maybe Plan
addOrRemoveInvigilatorExam -- whatToDo invigilator' (slotKey, slot') roomID' plan =
 = undefined
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
