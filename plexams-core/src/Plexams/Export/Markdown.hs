{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.Markdown
  ( planToMD
  ) where

import           Data.List             (intercalate)
import qualified Data.Map              as M
import           Data.Text             (unpack)
import           GHC.Exts              (groupWith)
import           Plexams.Export.Common
import           Plexams.Types

-- | Erzeugt eine Markdown-Version des aktuellen Plans
-- TODO: Tage gruppieren
planToMD :: Plan -> String
planToMD plan =
    "# Prüfungsplan " ++ unpack (semester (semesterConfig plan)) ++ "\n\n"
    ++ intercalate "\n\n" (map dayToMD slotList)
    ++ "\n\n## Noch nicht geplante (sortiert nach Anmeldezahlen)\n\n"
    ++ intercalate "\n\n"
                  (map examToMD $ unscheduledExamsSortedByRegistrations plan)

  where
    slotList = map (\days -> (fst $ head days, days))
             $ filter (not . null)
             $ groupWith (\((d,_),_) -> d)
             $ M.toAscList $ slots plan
    dayToMD ((d,_), slots') =
      "## " ++ dateString ((!!d) $ examDays $ semesterConfig plan) ++ "\n\n"
      ++ intercalate "\n\n" (map slotToMD slots')
    slotToMD (ds, slot') = "- " ++ slotToStr ds
      ++ maybe "" ((", Reserve: "++). unpack . invigilatorName)
                  (reserveInvigilator slot')
      ++ "\n\n"
      ++ intercalate "\n\n" (map (("    "++) . examToMD)
                                 $ M.elems $ examsInSlot slot')
    slotToStr (_,s) = slotsPerDay (semesterConfig plan)!!s ++ " Uhr"
    examToMD exam = "- " ++ show exam
