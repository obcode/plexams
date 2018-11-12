module Plexams.Generators.Schedule
  ( generateSchedule
  , scheduleExamsWithSameName
  ) where

import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import Plexams.PlanManip
import Plexams.Query
import Plexams.Types

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

-- findSlotForUnscheduledExam :: Integer -> Plan -> AddExamToSlot
-- findSlotForUnscheduledExam ancode plan = undefined
-- | Schedule exams with same name in the same slot.
--
-- This only works if one of them is already planned.
-- Ignores exams with same name that are planned in different slots.
scheduleExamsWithSameName :: Plan -> (Plan, [AddExamToSlot])
scheduleExamsWithSameName plan =
  let exams =
        filter (\(s, _) -> length s == 1) .
        map (filter isScheduled &&& filter isUnscheduled) $
        examsWithSameName $ setSlotsOnExams plan
      dayScheduled = fst . fromJust . slot
      slotScheduled = snd . fromJust . slot
      makePlanManips ([scheduled], unscheduled) =
        map
          (\exam ->
             AddExamToSlot
               (anCode exam)
               (dayScheduled scheduled)
               (slotScheduled scheduled))
          unscheduled
      makePlanManips _ = error "the impossible happened"
      planManips = concatMap makePlanManips exams
   in (applyAddExamToSlotListToPlan plan planManips, planManips)
