{-# LANGUAGE DeriveGeneric #-}
module Plexams.Export
  ( planToMD
  , planToJSONForZPA
  ) where

import           Data.Aeson
import           GHC.Generics
import           Plexams.Types

-- | Erzeugt eine Markdown-Version des aktuellen Plans
planToMD :: Plan -> String
planToMD plan =
    "# Prüfungsplan " ++ semesterName plan ++ "\n\n"
    ++ concatMap dayMD (realExamDays plan)
  where
    dayMD examDay = "## " ++ dateString examDay ++ "\n\n"
                    ++ concatMap slotMD (slotsOfDay examDay)
    slotMD slot = "- " ++ timeString slot ++ "\n\n"

data ZPAExam = ZPAExam
    { date :: String
    , time :: String
    }
  deriving (Generic)

instance ToJSON ZPAExam where
    toEncoding = genericToEncoding defaultOptions

planToJSONForZPA :: Plan -> Encoding
planToJSONForZPA = undefined
