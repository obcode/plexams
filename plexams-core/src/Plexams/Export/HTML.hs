{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.HTML
  ( planToHTMLTable
  ) where

import           Data.List             (nub)
import qualified Data.Map              as M
import           Data.Text             (unpack)
import           GHC.Exts              (groupWith)
import           Plexams.Export.Common
import           Plexams.Types

insideTag :: String -> String -> String
insideTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

planToHTMLTable :: Maybe [Ancode] -> Plan -> String
planToHTMLTable maybeExams plan =
        before
        ++ planToHTMLTable'
        ++ caption
        ++ insideTag "h2" "Noch zu planen"
        ++ unscheduledExamsToList
        ++ insideTag "h2" "Nicht von mir zu planen"
        ++ unscheduledExamsPlannedByOthers
        ++ after
  where
    sName = semester $ semesterConfig plan
    before =
      "<html><head><meta charset=\"utf-8\"/><title>Prüfungsplan "
     ++ unpack sName
     ++ "</title>"
     ++ "<script type=\"text/javascript\" src=\"plexams.js\"></script>"
     ++ "<link href=\"plexams.css\" rel=\"stylesheet\" type=\"text/css\" />"
     ++ "</head><body>"
     ++ "<h1>Prüfungsplan "
     ++ unpack sName
     ++ "</h1>\n"
    after = "<script src=\"https://code.jquery.com/jquery-latest.js\"></script>"
         ++ "<script>"
         ++ "$(\".tiptext\").mouseover(function() {\n"
         ++ "  $(this).children(\".description\").show();\n"
         ++ "}).mouseout(function() {\n"
         ++ "    $(this).children(\".description\").hide();\n"
         ++ "});\n"
         ++ "</script>"
         ++"</body></html>"
    caption = "<span class=\"tiptext\">Basisfarbe</span>\n"
           ++ "<span class=\"tiptext reExam\">Wiederholung</span>\n"
           ++ "<span class=\"tiptext activeExam\">Aktive Prüfung</span>\n"
           ++ "<span class=\"tiptext conflict\">Konflikte</span>\n"
           ++ concatMap ((\g -> "<span class=\"tiptext "++g++"\">"++g++"</span>\n")
                  . show) allDegrees

    planToHTMLTable' =
      let header = "" : map show (examDays $ semesterConfig plan)
          columns =  slotsPerDay $ semesterConfig plan
          showExams (idx, slot') = insideTag "i" (show idx)
              ++ concatMap showExam (M.elems $ examsInSlot slot')
          showExam exam = "<div class=\"tiptext"
                ++ (if reExam exam then " reExam " else " ")
                ++ unwords (nub $ map (show . groupDegree) (groups exam))
                ++ (if isActiveExam exam then " activeExam " else "")
                ++ (if isConflict exam then " conflict " else "")
                ++ "\">"
                ++ show (anCode exam)
                ++ "(" ++ show (registrations exam) ++ ")"
                ++ "<div class=\"description\">"
                ++ show exam
                ++ "</div></div>"
          isActiveExam exam = maybe False (elem $ anCode exam) maybeExams
          isConflict exam = anCode exam `elem` conflicts
          conflicts = maybe [] (concatMap mkConflicts) maybeExams
          mkConflicts ancode = maybe []
            ( concatMap ( M.keys
                          . M.findWithDefault M.empty ancode
                          . olOverlaps
                        )
              . overlaps
            )
            $ constraints plan
          slotsAsMatrix = zipWith (:) columns
               $ map (map showExams)
               $ groupWith (\((_,t),_) -> t) $ M.toAscList $ slots plan
      in insideTag "table"
          $ insideTag "tr" (concatMap (insideTag "td") header)
            ++ concatMap (insideTag "tr" . concatMap (insideTag "td"))
                         slotsAsMatrix
    plannedByOtherExams =
        filter (not . plannedByMe) $ M.elems $ unscheduledExams plan
    unscheduledExamsToList = insideTag "ol"
            $ concatMap (insideTag "li" . show)
            (unscheduledExamsSortedByRegistrations plan)
    unscheduledExamsPlannedByOthers = insideTag "ul"
            $ concatMap (insideTag "li" . show) plannedByOtherExams
