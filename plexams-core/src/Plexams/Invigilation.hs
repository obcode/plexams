module Plexams.Invigilation
  ( sumInvigilation
  ) where

import qualified Data.Map      as M
import           Plexams.Types

sumInvigilation :: Plan -> Integer
sumInvigilation plan =
  let sumExams =
        sum
        $ concatMap
          (\exam -> map ((+ duration exam) . deltaDuration) $ rooms exam)
        $ scheduledExams plan
      sumMasterAndOralExams =
        sum
        $ map (\i -> invigilatorOralExams i + invigilatorMaster i)
        $ M.elems
        $ invigilators plan
  in sumExams + sumMasterAndOralExams
