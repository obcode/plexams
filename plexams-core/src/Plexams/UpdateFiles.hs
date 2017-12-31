module Plexams.UpdateFiles
  ( updatePlanManipFile
  , updateInvigilationFile
  ) where

import qualified Data.ByteString as BSI
import qualified Data.Yaml as Y

import Plexams.Import
       (importAddInvigilatorToRoomOrSlotFromYAMLFile,
        importExamSlotsFromYAMLFile)
import Plexams.Types

updatePlanManipFile :: FilePath -> AddExamToSlot -> IO ()
updatePlanManipFile filepath examSlot = do
  examSlots <- importExamSlotsFromYAMLFile filepath
  let examSlots' = changeSlot examSlot examSlots
      examSlots'' = examSlotsToLists examSlots'
  BSI.writeFile filepath $ Y.encode examSlots''

examSlotsToList :: AddExamToSlot -> [Integer]
examSlotsToList exam =
  [ planManipAnCode exam
  , toInteger $ planManipDay exam
  , toInteger $ planManipSlot exam
  ]

examSlotsToLists :: Maybe [AddExamToSlot] -> Maybe [[Integer]]
examSlotsToLists = fmap . map $ examSlotsToList

changeSlot :: AddExamToSlot -> Maybe [AddExamToSlot] -> Maybe [AddExamToSlot]
changeSlot examSlot examSlots = fmap (++ [examSlot]) filtered
  where
    filtered = fmap filterExam examSlots
    filterExam = filter (\x -> planManipAnCode x /= planManipAnCode examSlot)

updateInvigilationFile :: FilePath -> AddInvigilatorToRoomOrSlot -> IO ()
updateInvigilationFile filepath newInvig@(AddInvigilatorToRoomOrSlot _ invigSlot invigRoom) = do
  invigilations <- importAddInvigilatorToRoomOrSlotFromYAMLFile filepath
  let invigilations' = fmap replaceInvigilation invigilations
  BSI.writeFile filepath $ Y.encode invigilations'
  where
    replaceInvigilation =
      (newInvig :) .
      filter
        (\(AddInvigilatorToRoomOrSlot _ invigSlot' invigRoom') ->
           invigSlot' /= invigSlot || invigRoom' /= invigRoom)
