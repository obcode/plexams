{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.Markdown
  ( planToMD
  ) where

import           Data.List             (intercalate)
import qualified Data.Map              as M
import           Plexams.Export.Common
import           Plexams.Types

-- | Erzeugt eine Markdown-Version des aktuellen Plans
-- TODO: Tage gruppieren
planToMD :: Plan -> String
planToMD plan =
    "# Prüfungsplan " ++ semester (semesterConfig plan) ++ "\n\n"
    ++ intercalate "\n\n" (map slotToMD slotList)
    ++ "\n\n## Noch nicht geplante (sortiert nach Anmeldezahlen)\n\n"
    ++ intercalate "\n\n"
                  (map examToMD $ unscheduledExamsSortedByRegistrations plan)

  where
    slotList = M.toAscList $ slots plan
    slotToMD (ds, slot) = dayAndSlotToString ds ++ "\n\n"
      ++ intercalate "\n\n" (map (("    "++) . examToMD) $ M.elems $ examsInSlot slot)
    dayAndSlotToString (d,s) = "- " ++ dayStr ++ ": " ++ slotStr ++ " Uhr"
      where dayStr = show $ (!!d) $ examDays $ semesterConfig plan
            slotStr = (!!s) $ slotsPerDay $ semesterConfig plan
    examToMD exam = "- " ++ show exam
