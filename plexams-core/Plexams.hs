module Plexams where

import           Plexams.Import
import           Plexams.Types

initSemesterConfigFromFile :: FilePath -> IO (Maybe SemesterConfig)
initSemesterConfigFromFile = importSemesterConfigFromJSONFile
