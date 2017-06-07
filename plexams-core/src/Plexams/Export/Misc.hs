{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.Misc
  ( semesterConfigAsString
  , exportAddExamToSlots
  , exportAddRoomToExams
  , exportHandicaps
  ) where

import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (intercalate)
import           GHC.Exts                   (sortWith)
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
  ++ "\n  seatsPlanned: " ++ show s ++
  (case d of Nothing -> ""
             Just dd -> "\n  deltaDuration: " ++ show dd)

--------------------------------------------------------------------------------
-- Export Handicaps for Lecturers
--------------------------------------------------------------------------------

exportHandicaps :: Plan -> String
exportHandicaps plan =
  intercalate "\n"
  $ map show
  $ sortWith lecturer
  $ filter (not . null . studentsWithHandicaps)
  $ scheduledExams
  $ setHandicapsOnScheduledExams plan
