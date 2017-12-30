{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Plexams.Statistics
  ( initialPlanStatistics
  , planStatistics
  ) where

import Control.Arrow (first)
import Data.List (intercalate, nub)
import qualified Data.Map as M
import Data.Text (unpack)
import GHC.Exts (groupWith, sortWith)
import Plexams.Invigilation
import Plexams.Query
import Plexams.Types

initialPlanStatistics :: Plan -> String
initialPlanStatistics plan =
  "# Statistiken für den initialen Plan\n\n" ++
  concatMap
    ($ plan)
    [shortGroupStatsToString, examsForLecturerers, examsWithSameNameString]

planStatistics :: Plan -> String
planStatistics plan =
  "\n\n# Statistiken für den aktuellen Plan\n\n" ++
  concatMap
    ($ plan)
    [ examsWithoutRegistrations
    , examGroupsCorrelationToString
    , lecturerExamDaysToString
    , invigilationInfo
    , invigilatorInfo
    , invigilatorsInfo
    , invigilatorsPerDayToString
    , invigilationsPerPersonToString
    ]

shortGroupStatsToString :: Plan -> String
shortGroupStatsToString =
  (header ++) .
  (++ footer) .
  concatMap (\(g, es) -> "-   " ++ myShow g ++ myShowExams es ++ "\n") .
  groupStats
  where
    header = "## Anzahl der Prüfungen pro Gruppe\n\n"
    footer = "\n"
    myShow g =
      let gStr = show g
      in gStr ++ ": " ++ replicate (4 - length gStr) ' '
    myShowLength es =
      let esStr = show $ length es
      in replicate (3 - length esStr) ' ' ++ esStr
    myShowExams [] = ""
    myShowExams (es:rest) =
      myShowLength es ++
      (if reExam $ head es
         then " Wiederholungsprüfungen"
         else " Prüfungen") ++
      myShowExams rest

groupStats :: Plan -> [(Group, [[Exam]])]
groupStats plan =
  let crossProduct = [(g, e) | e <- initialPlan plan, g <- groups e]
      groupList =
        map (\l -> (fst $ head l, groupWith reExam $ map snd l)) $
        groupWith fst crossProduct
  in groupList

examGroupsCorrelationToString :: Plan -> String
examGroupsCorrelationToString =
  (header ++) .
  (++ footer) .
  concatMap
    (\(g1, grps) ->
       "  " ++ show g1 ++ " -> " ++ intercalate " -> " (map show grps) ++ "\n") .
  examGroupsCorrelation
  where
    header =
      "## Gemeinsame Klausuren verschiedener Gruppen\n\n" ++
      "~~~~\ndigraph groups {\n"
    footer = "\n}\n~~~~\n\n"

examGroupsCorrelation :: Plan -> [(Group, [Group])]
examGroupsCorrelation plan =
  let groupsWithSameExam = concatMap groupGroups $ allExams plan
      groupGroups :: Exam -> [(Group, Group)]
      groupGroups exam =
        let grps = groups exam
        in [(g1, g2) | g1 <- grps, g2 <- grps, g1 /= g2]
  in map (\xs@((g1, _):_) -> (g1, nub $ map snd xs)) $
     groupWith fst groupsWithSameExam

examsWithoutRegistrations :: Plan -> String
examsWithoutRegistrations =
  (header ++) .
  intercalate "\n" .
  map (("-   " ++) . show) . filter ((== 0) . registrations) . allExams
  where
    header = "## Prüfungen ohne Anmeldungen\n\n"

examsForLecturerers :: Plan -> String
examsForLecturerers =
  ("## Anzahl Prüfungen pro Prüfer\n\n" ++) .
  (++ "\n\n") .
  intercalate "\n" .
  map
    (\es ->
       "-   " ++
       unpack (personShortName (fst (head es))) ++
       ": " ++
       show (length es) ++
       " Prüfungen (" ++ intercalate ", " (map (show . anCode . snd) es) ++ ")") .
  sortWith length . groupWith fst . map (\e -> (lecturer e, e)) . initialPlan

lecturerExamDaysToString :: Plan -> String
lecturerExamDaysToString =
  ("\n## Prüfungstage der Prüfer\n\n" ++) . show . examDaysPerLecturer

examsWithSameNameString :: Plan -> String
examsWithSameNameString =
  ("\n\n## Prüfungen mit gleichem Namen\n\n" ++) .
  intercalate "\n" .
  map
    (\exams' -> "-   " ++ name (head exams') ++ ": " ++ show (map anCode exams')) .
  examsWithSameName

invigilatorInfo :: Plan -> String
invigilatorInfo =
  ("\n\n## Infos zu Aufsichten\n\n" ++) .
  ("Legende: e=Exams, -=Excluded, w=Want, c=Can f=Freisemester, p=partTime" ++) .
  (", ot=overThis, ol=overLast, o=oralExams, m=master, pe=percent\n\n" ++) .
  concatMap (("\n-  " ++) . (++ "\n") . showInvigilator) .
  M.elems . invigilators
  where
    showInvigilator invigilator' =
      show (invigilatorID invigilator') ++
      " " ++
      unpack (invigilatorName invigilator') ++
      ": " ++
      "e" ++
      show (invigilatorExamDays invigilator') ++
      " -" ++
      show (invigilatorExcludedDays invigilator') ++
      " w" ++
      show (invigilatorWantDays invigilator') ++
      " c" ++
      show (invigilatorCanDays invigilator') ++
      " f" ++
      show (invigilatorFreeSemester invigilator') ++
      " p" ++
      show (invigilatorPartTime invigilator') ++
      " ot" ++
      show (invigilatorOvertimeThisSemester invigilator') ++
      " ol" ++
      show (invigilatorOvertimeLastSemester invigilator') ++
      " o" ++
      show (invigilatorOralExams invigilator') ++
      " m" ++
      show (invigilatorMaster invigilator') ++
      " pe" ++
      show (sumPercentInvigilator invigilator') ++
      "\n\n    todo: " ++
      show (invigilatorMinutesTodo invigilator') ++
      " already planned: " ++
      show (invigilatorsMinutesPlanned invigilator') ++
      " open: " ++
      show
        (invigilatorMinutesTodo invigilator' -
         invigilatorsMinutesPlanned invigilator')

invigilatorsInfo :: Plan -> String
invigilatorsInfo plan =
  let invigilators' = M.elems $ invigilators plan
      invigilatorMinutes =
        map
          (\i -> invigilatorMinutesTodo i - invigilatorsMinutesPlanned i)
          invigilators'
      sumInvigilationsTodo = sum $ map invigilatorMinutesTodo invigilators'
      sumInvigilationsPlanned =
        sum $ map invigilatorsMinutesPlanned invigilators'
      standardDeviation =
        sqrt
          (fromInteger
             (sum (map (^ 2) invigilatorMinutes) `div`
              toInteger (length invigilatorMinutes)))
  in "\n\n## Mittelwert und Standardabweichung der eingeplanten Minuten\n\n" ++
     "- Werte: " ++
     show invigilatorMinutes ++
     "\n\n" ++
     "- Summe aller Todos: " ++
     show sumInvigilationsTodo ++
     "\n\n" ++
     "- Summe aller Planungen: " ++
     show sumInvigilationsPlanned ++
     "\n\n" ++
     "- Mittelwert: " ++
     show (sum invigilatorMinutes `div` toInteger (length invigilatorMinutes)) ++
     "\n\n" ++ "- Standardabweichung: " ++ show standardDeviation ++ "\n\n"

invigilatorsPerDayToString :: Plan -> String
invigilatorsPerDayToString =
  ("\n## Aufsichten pro Tag\n\n" ++) .
  concatMap
    (\(day, (ws, cs)) ->
       "Tag " ++
       show day ++
       "\n\n" ++
       "-    wollen(" ++
       show (length ws) ++
       "):\n\n" ++
       concatMap (("    - " ++) . (++ "\n\n") . unpack . invigilatorName) ws ++
       "\n\n-    können(" ++
       show (length cs) ++
       "):\n\n" ++
       concatMap (("    - " ++) . (++ "\n\n") . unpack . invigilatorName) cs) .
  M.toList . mkInvigilatorsPerDay

invigilationInfo :: Plan -> String
invigilationInfo plan =
  let Invigilations sumExams sumReserve sumOralExams sumMaster sumLivecoding =
        mkInvigilations plan
      sumPercent = sumPercentAllInvigilators plan
      hundertPercentInMinutes' = hundertPercentInMinutes plan
      sumMasterAndOralExamsAndLivecoding =
        sumOralExams + sumMaster + sumLivecoding
  in "\n\n## Infos zu Aufsichtsbedarf\n\n" ++
     "- Summe Aufsichten: " ++
     show sumExams ++
     " Minuten\n\n" ++
     "- Summe Reserveaufsichten: " ++
     show sumReserve ++
     " Minuten\n\n" ++
     "- Summe Beisitz/Master/Livecoding: " ++
     show sumMasterAndOralExamsAndLivecoding ++
     " Minuten\n\n" ++
     "- Summe der Prozente aller Aufsichten: " ++
     show sumPercent ++
     "%\n\n" ++
     "- Hundert Prozent entspricht also " ++
     show hundertPercentInMinutes' ++ " Minuten\n\n"

invigilationsPerPersonToString :: Plan -> String
invigilationsPerPersonToString plan =
  ("\n\n# Einplanungen Aufsichten\n\n" ++) $
  intercalate "\n\n" $
  map
    ((\(maybeName, invigilations') ->
        case maybeName of
          Nothing -> "- ???: " ++ show invigilations'
          Just name' ->
            "- " ++
            name' ++
            "\n" ++
            intercalate "\n\n" (map (("    - " ++) . show) invigilations') ++
            "\n\n") .
     first (fmap (unpack . invigilatorName) . flip M.lookup (invigilators plan))) $
  M.toList $ invigilationsPerPerson plan
