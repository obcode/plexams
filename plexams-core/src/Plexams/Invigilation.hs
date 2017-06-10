module Plexams.Invigilation
  ( sumInvigilation
  , invigilatorsPerDay
  ) where

import qualified Data.Map      as M
import           Plexams.Types

sumInvigilation :: Plan -> (Integer, Integer, Integer)
sumInvigilation plan =
  let sumExams =
        sum
        $ concatMap
          (\exam -> map ((+ duration exam) . deltaDuration) $ rooms exam)
        $ scheduledExams plan
      sumReserve =
        sum
        $ map (\slot' -> if M.null (examsInSlot slot')
                         then 0
                         else 60)
        $ M.elems
        $ slots plan
      sumMasterAndOralExams =
        sum
        $ map (\i -> invigilatorOralExams i + invigilatorMaster i)
        $ M.elems
        $ invigilators plan
  in (sumExams, sumReserve, sumMasterAndOralExams)

                                         -- ( want      , can        )
invigilatorsPerDay :: Plan -> M.Map DayIndex ([Invigilator], [Invigilator])
invigilatorsPerDay =
  foldr (\(invigilator', want, day) ->
      M.alter (\invigilators' -> Just $
        case invigilators' of
          Nothing            -> if want
                                then ([invigilator'], [])
                                else ([], [invigilator'])
          Just (wants, cans) -> if want
                                then (invigilator':wants, cans)
                                else (wants, invigilator':cans)
      ) day
    )
    M.empty
  . concatMap (\invigilator' ->
        map (\d -> (invigilator', True,  d)) (invigilatorWantDays invigilator')
     ++ map (\d -> (invigilator', False, d)) (invigilatorCanDays  invigilator')
    )
  . M.elems
  . invigilators
