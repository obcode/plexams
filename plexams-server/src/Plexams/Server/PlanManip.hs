module Plexams.Server.PlanManip
  ( updateManipFile
  ) where

import qualified Data.ByteString   as BSI
import qualified Data.Yaml         as Y
import           Plexams.Import    (importExamSlotsFromYAMLFile)
import           Plexams.Types

-- TODO: move to core

updateManipFile :: FilePath -> AddExamToSlot -> IO ()
updateManipFile outfile exam = do
  test1 <- importExamSlotsFromYAMLFile outfile
  let changed = changeSlot exam test1
      list = examSlotsToLists changed
  BSI.writeFile outfile $ Y.encode list

examSlotsToList :: AddExamToSlot -> [Integer]
examSlotsToList exam = [planManipAnCode exam
                        , toInteger $ planManipDay exam
                        , toInteger $ planManipSlot exam]

examSlotsToLists :: Maybe [AddExamToSlot] -> Maybe [[Integer]]
examSlotsToLists = fmap . map $ examSlotsToList

changeSlot :: AddExamToSlot -> Maybe [AddExamToSlot] -> Maybe [AddExamToSlot]
changeSlot exam exams' = fmap (++ [exam]) filtered
  where
    filtered = fmap filterExam exams'
    filterExam = filter (\x -> planManipAnCode x /= planManipAnCode exam)
