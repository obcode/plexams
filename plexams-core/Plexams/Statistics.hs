module Plexams.Statistics
    ( initialPlanStatistics
    , planStatistics
    ) where

import           Data.List     (intercalate, nub)
import qualified Data.Map      as M
import           GHC.Exts      (groupWith, sortWith)
import           Plexams.Query
import           Plexams.Types

initialPlanStatistics :: [Exam] -> String
initialPlanStatistics exams =
  "# Statistiken für den initialen Plan\n\n"
    ++ concatMap ($ exams)
        [ shortGroupStatsToString
        , examsForLecturerers
        , examsWithSameName
        ]

-- planStatistics :: Plan -> String
planStatistics plan =
  "\n\n# Statistiken für den aktuellen Plan\n\n"
    ++ concatMap ($ plan)
        [ examGroupsCorrelationToString
        , lecturerExamDaysToString
        ]

shortGroupStatsToString :: [Exam] -> String
shortGroupStatsToString =
    (header++)
    . (++footer)
    . concatMap (\(g, es) -> "-   " ++ myShow g ++ myShowExams es ++ "\n")
    . groupStats
  where
      header = "## Anzahl der Prüfungen pro Gruppe\n\n"
      footer = "\n"
      myShow g = let gStr = show g
                 in gStr ++ ": " ++ replicate (4 - length gStr) ' '
      myShowLength es = let esStr = show $ length es
                        in replicate (3 - length esStr) ' ' ++ esStr
      myShowExams [] = ""
      myShowExams (es:rest) = myShowLength es ++ (if reExam $ head es
                                                  then " Wiederholungsprüfungen"
                                                  else " Prüfungen"
                                                 ) ++ myShowExams rest

groupStats :: [Exam] -> [(Group, [[Exam]])]
groupStats exams =
    let crossProduct = [ (g, e) | e <- exams, g <- groups e ]
        groupList = map (\l -> (fst $ head l, groupWith reExam $ map snd l))
                        $ groupWith fst crossProduct
    in groupList

examGroupsCorrelationToString :: Plan -> String
examGroupsCorrelationToString =
    (header++)
    . (++footer)
    . concatMap (\(g1, grps) -> "  " ++ show g1 ++ " -> "
                              ++ intercalate " -> " (map show grps) ++ "\n")
    . examGroupsCorrelation
  where header = "## Gemeinsame Klausuren verschiedener Gruppen\n\n"
                ++ "digraph groups {\n"
        footer = "\n}\n\n"

examGroupsCorrelation :: Plan -> [(Group, [Group])]
examGroupsCorrelation plan =
        let groupsWithSameExam = concatMap groupGroups $ allExams plan
            groupGroups :: Exam -> [(Group, Group)]
            groupGroups exam =
                let grps = groups exam
                in [(g1,g2) | g1 <- grps, g2 <- grps, g1 /= g2]
        in map (\xs@((g1,_):_) -> (g1, nub $ map snd xs))
               $ groupWith fst groupsWithSameExam

examsForLecturerers :: [Exam] -> String
examsForLecturerers =
  ("## Anzahl Prüfungen pro Prüfer\n\n"++)
  . intercalate "\n"
  . map (\es -> "-   "
                ++ personShortName (fst (head es))
                ++ ": " ++ show (length es)
                ++ " Prüfungen ("
                ++ intercalate ", " (map (show . anCode . snd) es)
                ++ ")"
        )
  . sortWith length
  . groupWith fst
  . map (\e -> (lecturer e, e))

lecturerExamDaysToString :: Plan -> String
lecturerExamDaysToString =
    ("\n## Prüfungstage der Prüfer\n\n"++)
    . intercalate "\n"
    . map (\(person, listOfDays) -> "-   "
                                    ++ personShortName person
                                    ++ ": "
                                    ++ intercalate ", " (map show listOfDays))
    . lecturerExamDays

examsWithSameName :: [Exam] -> String
examsWithSameName =
  ("\n## Prüfungen mit gleichem Namen\n\n"++)
  . intercalate "\n"
  . map (\exams -> "-   " ++ name (head exams)
                ++ ": " ++ show (map anCode exams))
  . filter ((>1) . length)
  . groupWith name
