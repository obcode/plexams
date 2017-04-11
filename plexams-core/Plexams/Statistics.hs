module Plexams.Statistics
    ( planStatistics
    ) where

import           Data.List     (intercalate, nub)
import qualified Data.Map      as M
import           GHC.Exts      (groupWith)
import           Plexams.Query
import           Plexams.Types

-- planStatistics :: Plan -> String
planStatistics plan = concat -- Map toString
    [ shortGroupStatsToString plan
    , examGroupsCorrelationToString plan
    ]

shortGroupStatsToString :: Plan -> String
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

groupStats :: Plan -> [(Group, [[Exam]])]
groupStats plan =
    let crossProduct = [ (g, e) | e <- allExams plan, g <- groups e ]
        groupList = map (\l -> (fst $ head l, groupWith reExam $ map snd l)) $ groupWith fst crossProduct
    in groupList

examGroupsCorrelationToString :: Plan -> String
examGroupsCorrelationToString =
    (header++)
    . concatMap (\(g1, grps) -> "-   " ++ show g1 ++ " -> "
                              ++ intercalate ", " (map show grps) ++ "\n")
    . examGroupsCorrelation
  where header = "## Gemeinsame Klausuren verschiedener Gruppen\n\n"

examGroupsCorrelation :: Plan -> [(Group, [Group])]
examGroupsCorrelation plan =
        let groupsWithSameExam = concatMap groupGroups $ allExams plan
            groupGroups :: Exam -> [(Group, Group)]
            groupGroups exam =
                let grps = groups exam
                in [(g1,g2) | g1 <- grps, g2 <- grps, g1 /= g2]
        in map (\xs@((g1,_):_) -> (g1, nub $ map snd xs)) $ groupWith fst groupsWithSameExam
