{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.Misc
  ( semesterConfigAsString
  , exportAddExamToSlots
  , exportAddRoomToExams
  , exportAddInvigilatorToRoomOrSlot
  , exportHandicaps
  ) where

import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (intercalate)
import           GHC.Exts                   (sortWith)
-- import           Plexams.PlanManip          (setHandicapsOnScheduledExams)
import           Plexams.Types

--------------------------------------------------------------------------------
-- Print SemesterConfig
--------------------------------------------------------------------------------

semesterConfigAsString :: Plan -> String
semesterConfigAsString = unpack . encodePretty . semesterConfig

--------------------------------------------------------------------------------
-- Export AddExamToSlot to Yaml
--------------------------------------------------------------------------------

exportAddExamToSlots :: [AddExamToSlot] -> String
exportAddExamToSlots = intercalate "\n" . map exportAddExamToSlot

exportAddExamToSlot :: AddExamToSlot -> String
exportAddExamToSlot (AddExamToSlot a d s) =
  "- [" ++ show a ++ ", " ++ show d ++ ", " ++ show s ++ "]"

--------------------------------------------------------------------------------
-- Export AddRoomToExam to Yaml
--------------------------------------------------------------------------------

exportAddRoomToExams :: [AddRoomToExam] -> String
exportAddRoomToExams = intercalate "\n" . map exportAddRoomToExam

exportAddRoomToExam :: AddRoomToExam -> String
exportAddRoomToExam (AddRoomToExam a r s d) =
       "- ancode: " ++ show a
  ++ "\n  room: " ++ r
  ++ "\n  studentsInRoom: " ++ show s ++
  (case d of Nothing -> ""
             Just dd -> "\n  deltaDuration: " ++ show dd)

--------------------------------------------------------------------------------
-- Export AddRoomToExam to Yaml
--------------------------------------------------------------------------------

exportAddInvigilatorToRoomOrSlot :: [AddInvigilatorToRoomOrSlot] -> String
exportAddInvigilatorToRoomOrSlot = intercalate "\n"
                                  . map exportAddInvigilatorToRoomOrSlot'

exportAddInvigilatorToRoomOrSlot' :: AddInvigilatorToRoomOrSlot -> String
exportAddInvigilatorToRoomOrSlot' (AddInvigilatorToRoomOrSlot i (d,s) mR) =
       "- id: " ++ show i
  ++ "\n  slot: [" ++ show d ++ "," ++ show s ++ "]"
  ++ (case mR of Nothing -> ""
                 Just r  -> "\n  room: " ++ r)

--------------------------------------------------------------------------------
-- Export Handicaps for Lecturers
--------------------------------------------------------------------------------

exportHandicaps :: Plan -> String
exportHandicaps plan =
  intercalate "\n"
  $ map show
  $ sortWith lecturer
  $ filter (not . null . handicapStudents)
  $ scheduledExams plan
  -- $ setHandicapsOnScheduledExams plan
