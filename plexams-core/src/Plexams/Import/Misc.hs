{-# LANGUAGE OverloadedStrings #-}

module Plexams.Import.Misc
  ( importZPAExamsFromJSONFile
  ) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import Plexams.Types

--------------------------------------------------------------------------------
-- ZPAExams from JSON file
--------------------------------------------------------------------------------
importZPAExamsFromJSONFile :: FilePath -> IO (Maybe [ZPAExam])
importZPAExamsFromJSONFile = fmap decode . BS.readFile
