module Plexams.Generators.Schedule
  ( generateSchedule
  , scheduleExamsWithSameName
  )
where

import           Control.Arrow                  ( (&&&) )
-- import           Data.List                      ( intercalate
--                                                   find
                                                -- , nub
                                                -- , (\\)
                                                -- )
import           Data.Maybe                     ( fromJust )
--                                                 , mapMaybe
--                                                 )
-- import qualified Data.Map                      as M
import           Plexams.PlanManip
import           Plexams.Query
import           Plexams.Types

{- 
Schritte:
- Metrik: e1 >= e2 <=> Anzahl Anmeldungen e1 >= Anzahl Anmeldungen e2
    - größere Prüfungen weiter vorne?
- bereits feststehende Slots respektieren
- Trennen: GO und nicht-GO
- zuerst GO, größte in die Vormittagsslots um 08:30
- jetzt sind GO festgelegt
- Rest in Cluster einteilen
    - Cluster getrennt planen
- nur Tage festlegen, max 3 Slots pro Tag
- haben zwei Prüfungen ...?

andere Idee: für jeden Studierenden einen eigenen Plan in dem die Prüfungen
immer in der gleichen Reihenfolge sind. => vermutlich nicht zielführend

- vielleicht Optimierung eines bestehenden Planes.
-}

generateSchedule :: Plan -> (Plan, [AddExamToSlot])
generateSchedule _ = undefined

-- findSlotForUnscheduledExam :: Integer -> Plan -> Maybe AddExamToSlot
-- findSlotForUnscheduledExam ancode plan =
--   case M.lookup ancode (unscheduledExams plan) of
--     Nothing -> Nothing
--     Just _  -> undefined

-- findDayForExam :: Exam -> Plan -> ([Exam], DayIndex)
-- findDayForExam exam plan =
--   let
--     examGroup = exam : filter ((`elem` sameSlot exam) . anCode) (allExams plan)
--     conflictingAncodesForGroup =
--       M.toList $ M.unionsWith (+) $ map conflictingAncodes examGroup
--     scheduledExams'  = scheduledExams plan
--     conflictingSlots = mapMaybe
--       (\(ac, count) ->
--         maybe
--             Nothing
--             (\e -> case slot e of
--               Nothing    -> Nothing
--               Just slot' -> Just (slot', count)
--             )
--           $ find ((== ac) . anCode) scheduledExams'
--       )
--       conflictingAncodesForGroup
--     allDays         = [0 .. maxDayIndex plan]
--     conflictingDays = nub $ map (fst . fst) conflictingSlots
--     impossibleDays  = if any isGOExam examGroup
--       then nub $ map fst (goSlots $ semesterConfig plan)
--       else []
--     possibleDays = allDays \\ impossibleDays
--     daysWithoutConflict :: [(DayIndex, Bool)]
--     daysWithoutConflict = map (\i -> (i, i `notElem` conflictingDays)) allDays
--     daysWithoutConflictsOnAdjacentDays = undefined
--   in
--     undefined

-- SlotRating:
-- Für jede Überschneidung, die bereits gescheduled ist:
--    Anzahl Studierende dieser Überschneidung * irgendwas für Slot-Abstand * irgendwas für Tag-Abstand
--    Frühere Slots müssen besser bewertet werden
--    Slots dürfen nicht zu voll werden! Anzahl der Räume beachten!!!
-- größte Zahl gewinnt!
-- type SlotRating = Int

-- findSlotsForExam
--   :: Exam -> Plan -> ([Exam], [((DayIndex, SlotIndex), SlotRating)])
-- findSlotsForExam exam plan
--   = let
--       examGroup =
--         exam : filter ((`elem` sameSlot exam) . anCode) (allExams plan)
--       alreadyPlannedSlot = mapMaybe slot examGroup
--       feasibleSlots      = filter slotIsFeasible $ M.toList $ slots plan
--       slotIsFeasible _ = undefined
--     in
--       case alreadyPlannedSlot of
--         []  -> undefined -- planen
--         [s] -> (examGroup, [(s, maxBound)])
--         _ ->
--           error $ "error: sameSlot exams in different Rooms: \n" ++ intercalate
--             "\n"
--             (map show examGroup)

-- | Schedule exams with same name in the same slot.
--
-- This only works if one of them is already planned.
-- Ignores exams with same name that are planned in different slots.
scheduleExamsWithSameName :: Plan -> (Plan, [AddExamToSlot])
scheduleExamsWithSameName plan =
  let exams =
        filter (\(s, _) -> length s == 1)
          . map (filter isScheduled &&& filter isUnscheduled)
          $ examsWithSameName
          $ setSlotsOnExams plan
      dayScheduled  = fst . fromJust . slot
      slotScheduled = snd . fromJust . slot
      makePlanManips ([scheduled], unscheduled) = map
        (\exam -> AddExamToSlot (anCode exam)
                                (dayScheduled scheduled)
                                (slotScheduled scheduled)
        )
        unscheduled
      makePlanManips _ = error "the impossible happened"
      planManips = concatMap makePlanManips exams
  in  (applyAddExamToSlotListToPlan plan planManips, planManips)
