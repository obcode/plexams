module Plexams.Invigilation
  ( sumInvigilation
  , invigilatorsPerDay
  , sumPercentInvigilator
  , sumPercentAllInvigilators
  , hundertPercentInMinutes
  , removeInvigilatorsWithEnough
  , invigilatorsWithMinutesPlanned
  , invigilatorAddMinutes
  , invigilationsPerPerson
  ) where

import           Data.List     (nub)
import qualified Data.Map      as M
import           Data.Maybe    (catMaybes, mapMaybe)
import           GHC.Exts      (groupWith)
import           Plexams.Types

allSumInvigilation :: Plan -> Integer
allSumInvigilation plan =
  let (e,r,o) = sumInvigilation plan
  in e + r + o

minutesForReserve :: Integer
minutesForReserve = 60

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
                         else minutesForReserve)
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

sumPercentInvigilator :: Invigilator -> Integer
sumPercentInvigilator invigilator' =
  let percent = 100 * invigilatorPartTime invigilator'
      overthis = if invigilatorOvertimeThisSemester invigilator' == 1.5
                 then 0.5
                 else 0
      overlast = if invigilatorOvertimeLastSemester invigilator' == 1.5
                 then 0.5
                 else 0
      freeSemester = invigilatorFreeSemester invigilator'
      percent' = (1 + overthis - freeSemester - overlast) * percent
   in if percent' < 0 then 0 else round percent'

sumPercentAllInvigilators :: Plan -> Integer
sumPercentAllInvigilators =
  sum . map sumPercentInvigilator . M.elems . invigilators

hundertPercentInMinutes :: Plan -> Integer
hundertPercentInMinutes plan =
  let overallSum = allSumInvigilation plan
      sumPercentInvigilators = sumPercentAllInvigilators plan
  in overallSum * 100 `div` sumPercentInvigilators

removeInvigilatorsWithEnough :: Plan -> Plan
removeInvigilatorsWithEnough plan =
  let hundertPercentInMinutes' = hundertPercentInMinutes plan
  in plan
    { invigilators = M.filter (\invigilator' ->
        invigilatorOralExams invigilator' + invigilatorMaster invigilator'
        < sumPercentInvigilator invigilator' * hundertPercentInMinutes'
            `div` 100
        ) $ invigilators plan
    }

invigilatorsWithMinutesPlanned :: Plan -> M.Map PersonID Integer
invigilatorsWithMinutesPlanned plan =
  let reserves :: [(PersonID, Integer)]
      reserves = mapMaybe
        (fmap (\invigilator' -> (invigilatorID invigilator', minutesForReserve))
          . reserveInvigilator) $ M.elems $ slots plan
      examsDurations :: [(PersonID, Integer)]
      examsDurations =
        concatMap
          ( concatMap (\exam' ->
              concatMap (\room' ->
                          maybe []
                                (\invigilator' ->
                                  [( invigilatorID invigilator'
                                  , deltaDuration room' + duration exam'
                                  )]
                                )
                                $ invigilator room'
                        ) $ rooms exam'
            )
            . M.elems
            . examsInSlot
          ) $ slots plan
  in M.fromList
    $ map (\groupedInvigilator ->
            ( fst $ head groupedInvigilator
            , sum $ map snd groupedInvigilator
            )
          )
    $ groupWith fst
    $ reserves ++ examsDurations

invigilatorAddMinutes :: Plan -> Plan
invigilatorAddMinutes plan =
  let plan' = removeInvigilatorsWithEnough plan
      invigilatorsWithMinutesPlanned' = invigilatorsWithMinutesPlanned plan
      hundertPercentInMinutes' = hundertPercentInMinutes plan
      invigilatorsWithMinutes = M.map (\invigilator' ->
          invigilator'
            { invigilatorMinutesTodo =
                ((sumPercentInvigilator invigilator' * hundertPercentInMinutes')
                `div` 100)
                - invigilatorOralExams invigilator'
                - invigilatorMaster invigilator'
            , invigilatorsMinutesPlanned =
                M.findWithDefault 0 (invigilatorID invigilator')
                                    invigilatorsWithMinutesPlanned'
            }
        ) $ invigilators plan'
  in  plan'
        { invigilators = invigilatorsWithMinutes
        }

invigilatorsPlanned :: Plan -> [PersonID]
invigilatorsPlanned plan =
  let reserveInvigilators =
        mapMaybe (fmap invigilatorID . reserveInvigilator)
                 $ M.elems $ slots plan
      examInvigilators = catMaybes
        $ concatMap (map (fmap invigilatorID .  invigilator) . rooms)
        $ concatMap (M.elems . examsInSlot) $ M.elems $ slots plan
  in nub (reserveInvigilators ++ examInvigilators)

invigilationsPerPerson :: Plan -> M.Map PersonID [Invigilation]
invigilationsPerPerson plan =
  let invigilatorsPlanned' = invigilatorsPlanned plan
      mkInvigilations invigilatorID' =
        mkReserveInvigilations invigilatorID'
        ++ mkExamInvigilations invigilatorID'
      mkReserveInvigilations invigilatorID' =
        map (\((d,s),_) ->
              Invigilation
                { invigilationInvigilatorID = invigilatorID'
                , invigilationDay = d
                , invigilationSlot = s
                , invigilationRoom = Nothing
                , invigilationDuration = minutesForReserve
                }
            )
        $ filter ((== Just invigilatorID')
                 . fmap invigilatorID
                 . reserveInvigilator
                 . snd)
        $ M.toList
        $ slots plan
      mkExamInvigilations invigilatorID' =
        map (\((d,s),((roomID', duration'),_)) ->
              Invigilation
                { invigilationInvigilatorID = invigilatorID'
                , invigilationDay = d
                , invigilationSlot = s
                , invigilationRoom = Just roomID'
                , invigilationDuration = duration'
                }
            )
        $ filter ((== Just invigilatorID')
                 . fmap invigilatorID
                 . snd
                 . snd)
        $ concatMap (\(idx, slot') ->
                      concatMap (\e ->
                        map (\r ->
                              (idx, ( (roomID r, duration e + deltaDuration r)
                                    , invigilator r))
                            )
                            $ rooms e
                      )
                      $ M.elems
                      $ examsInSlot slot'
                    )
        $ M.toList
        $ slots plan
  in M.fromList
    $ map (\invigilatorID' ->
            ( invigilatorID'
            , mkInvigilations invigilatorID'
            )
          )
          invigilatorsPlanned'
