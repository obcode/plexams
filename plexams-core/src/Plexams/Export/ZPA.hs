{-# LANGUAGE OverloadedStrings #-}
module Plexams.Export.ZPA
  ( planToZPA
  ) where

import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map                   as M
import           GHC.Exts                   (sortWith)
import           Plexams.Types

--------------------------------------------------------------------------------
-- Export for ZPA
--------------------------------------------------------------------------------

examToZPAExam :: Plan -> Maybe Invigilator -> Exam -> ZPAExam
examToZPAExam plan reserveInvigilator' exam = ZPAExam
    { zpaExamAnCode = anCode exam
    , zpaExamDate = examDateAsString exam plan
    , zpaExamTime = examSlotAsString exam plan
    , zpaTotalNumber = registrations exam
    , zpaExamReserveInvigilatorId = maybe 0 invigilatorID reserveInvigilator'
    , zpaExamRooms = map (roomToZPARoom $ duration exam) $ rooms exam
    }

roomToZPARoom :: Integer -> Room -> ZPARoom
roomToZPARoom duration' room = ZPARoom
    { zpaRoomNumber = roomID room
    , zpaRoomInvigilatorId = maybe 0 invigilatorID $ invigilator room
    , zpaRoomReserveRoom = reserveRoom room
    , zpaRoomHandicapCompensation = handicapCompensation room
    , zpaRoomDuration = duration' + deltaDuration room
    , zpaRoomNumberStudents = seatsPlanned room
    }

planToZPAExams :: Plan -> [ZPAExam]
planToZPAExams plan = map (uncurry (examToZPAExam plan))
               $ scheduledExamsWithReserveInvigilator plan

scheduledExamsWithReserveInvigilator :: Plan -> [(Maybe Invigilator, Exam)]
scheduledExamsWithReserveInvigilator =
    sortWith (anCode . snd)
    . filter (plannedByMe . snd)
    . concatMap (reserveAndExams . snd)
    . M.toList
    . slots
    . setSlotsOnExams
  where reserveAndExams slot' =
          map (\exam -> (reserveInvigilator slot', exam))
              $ M.elems $ examsInSlot slot'

planToZPA :: Plan -> String
planToZPA = unpack . encodePretty' config . planToZPAExams
  where
    config = defConfig { confCompare = keyOrder [ "anCode"
                                                , "date"
                                                , "time"
                                                , "total_number"
                                                , "reserveInvigilator_id"
                                                , "rooms"
                                                , "number"
                                                , "invigilator_id"
                                                , "numberStudents"
                                                , "reserveRoom"
                                                , "handicapCompensation"
                                                , "duration"
                                                ]
                       }
