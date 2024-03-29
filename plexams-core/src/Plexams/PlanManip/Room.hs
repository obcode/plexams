module Plexams.PlanManip.Room
  ( applyAddRoomToExamListToPlan
  ) where

import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import GHC.Exts (sortWith)

import Plexams.Types

applyAddRoomToExamListToPlan :: Plan -> [AddRoomToExam] -> Plan
applyAddRoomToExamListToPlan = foldr applyPlanManipToPlan
  where
    applyPlanManipToPlan (AddRoomToExam a n s dd nta r) =
      addRoomToExam a n s dd nta r

-- TODO: Reserve nach oben eingeplant?
addRoomToExam ::
     Integer
  -> String
  -> [Text]
  -> Maybe Integer
  -> Bool
  -> Bool
  -> Plan
  -> Plan
addRoomToExam ancode roomName studentsInRoom' maybeDeltaDuration nta reserve plan =
  if ancode `M.member` unscheduledExams plan
    then plan -- a room cannot be added to an unscheduled exam
       -- exam is scheduled (or unknown)
    else let exam =
               head -- should never fail
                $
               mapMaybe (M.lookup ancode . examsInSlot) $
               M.elems $ slots $ setSlotsOnExams plan
             (studsInRoom, otherStuds) =
               partition ((`elem` studentsInRoom') . studentMtknr) $
               registeredStudents exam
             availableRoom =
               head -- should never fail
                $
               filter ((== roomName) . availableRoomName) $
               availableRooms $ semesterConfig plan
             room =
               Room
               { roomID = roomName
               , maxSeats = availableRoomMaxSeats availableRoom
               , deltaDuration = fromMaybe 0 maybeDeltaDuration
               , invigilator = Nothing
               , reserveRoom =
                   reserve ||
                   not nta &&
                   (toInteger (length studsInRoom) < registrations exam `div` 5)
               , handicapCompensation =
                   availableRoomHandicap availableRoom || nta
               , studentsInRoom = sortWith studentFamilyname studsInRoom
               }
    -- step 3: add room to exam and put new exam into correct slot
         in plan
            { slots =
                M.alter
                  (\(Just slot') ->
                     Just
                       slot'
                       { examsInSlot =
                           M.alter
                             (\(Just exam') ->
                                Just
                                  exam'
                                  { rooms = room : rooms exam'
                                  , registeredStudents = otherStuds
                                  })
                             ancode $
                           examsInSlot slot'
                       })
                  (fromMaybe (0, 0) $ slot exam) $
                slots plan
            }
