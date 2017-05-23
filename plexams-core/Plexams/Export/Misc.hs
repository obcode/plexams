{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.Misc
  ( semesterConfigAsString
  , exportPlanManips
  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (intercalate)
import           Plexams.Types

--------------------------------------------------------------------------------
-- Print SemesterConfig
--------------------------------------------------------------------------------

instance ToJSON SemesterConfig where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON AvailableRoom where
    toEncoding = genericToEncoding defaultOptions

semesterConfigAsString :: Plan -> String
semesterConfigAsString = unpack . encodePretty . semesterConfig

--------------------------------------------------------------------------------
-- Export PlanManips to Yaml
--------------------------------------------------------------------------------

exportPlanManips :: [PlanManip] -> String
exportPlanManips = intercalate "\n" . map exportPlanManip

exportPlanManip :: PlanManip -> String
exportPlanManip (AddExamToSlot a d s) =
  "- [" ++ show a ++ ", " ++ show d ++ ", " ++ show s ++ "]"
