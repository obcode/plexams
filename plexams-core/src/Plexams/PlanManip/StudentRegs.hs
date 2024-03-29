{-# LANGUAGE OverloadedStrings #-}
module Plexams.PlanManip.StudentRegs
  ( addStudentRegistrationsToPlan
  )
where

import           Data.List                      ( (\\)
                                                , sort
                                                )
import qualified Data.Map                      as M
import           GHC.Exts                       ( groupWith )

import           Plexams.Types

addStudentRegistrationsToPlan :: StudentsWithRegs -> Plan -> Plan
addStudentRegistrationsToPlan studentsWithRegs plan = plan
  { slots            = slots'
  , unscheduledExams = unscheduledExams'
  , students         =
    M.map
      (\stud -> stud
        { studentAncodes = sort $ filter (`elem` allAncodes) $ studentAncodes
                             stud
        }
      )
      studentsWithRegs
  }
 where
  unscheduledExams' =
    addStudentRegistrationsToExamsMap studentsWithRegs allAncodes
      $ unscheduledExams plan
  slots' =
    M.map
        (\s -> s
          { examsInSlot =
            addStudentRegistrationsToExamsMap studentsWithRegs allAncodes
              $ examsInSlot s
          }
        )
      $ slots plan
  allAncodes = map anCode $ initialPlan plan

addStudentRegistrationsToExamsMap
  :: StudentsWithRegs -> [Ancode] -> M.Map Ancode Exam -> M.Map Ancode Exam
addStudentRegistrationsToExamsMap studentsWithRegs allAncodes examsMap =
  M.map
      (\e -> e
        { conflictingAncodes =
          M.filterWithKey (\k _ -> k `elem` allAncodes \\ [anCode e])
            $ conflictingAncodes e
        , registeredGroups   = sumRegisteredGroups $ registeredGroups e
        }
      )
    $ foldr insertStudentReg examsMap'
    $ M.elems studentsWithRegs
 where
  examsMap' = M.map
    (\e -> case unregisteredStudents e of
      0     -> e
      count -> e
        { registeredStudents =
          registeredStudents e ++ replicate
            (fromInteger count)
            (StudentWithRegs "_______"
                             "Anmeldedaten"
                             "keine"
                             "UN"
                             [anCode e]
                             Nothing
            )
        , registeredStudentsCount = count + registeredStudentsCount e
        , registeredGroups = RegisteredGroup "UN" count : registeredGroups e
        }
    )
    examsMap

  sumRegisteredGroups regGroups =
    map
        (\(RegisteredGroup gName i : gs) ->
          RegisteredGroup gName (i + sum (map registeredGroupStudents gs))
        )
      $ groupWith registeredGroupDegree regGroups
  insertStudentReg studentWithReg examsMap'' =
    foldr
        (M.alter $ fmap $ \e -> e
          { registeredStudents      = studentWithReg' : registeredStudents e
          , registeredStudentsCount = 1 + registeredStudentsCount e
          , registeredGroups = RegisteredGroup (studentGroup studentWithReg) 1
                                 : registeredGroups e
          , conflictingAncodes      = foldr (M.alter (Just . maybe 1 (+ 1)))
                                            (conflictingAncodes e)
                                            (studentAncodes studentWithReg)
          , handicapStudents        = maybe
                                        (handicapStudents e)
                                        (const $ studentWithReg : handicapStudents e)
                                        (studentHandicap studentWithReg')
          }
        )
        examsMap''
      $ studentAncodes studentWithReg
   where
    studentWithReg' = studentWithReg
      { studentAncodes = filter (`elem` M.keys examsMap)
                           $ studentAncodes studentWithReg
      }
