{-# LANGUAGE OverloadedStrings #-}

module Plexams.Invigilation
  ( mkInvigilations
  , invigilatorsPerDay
  , sumPercentInvigilator
  , sumPercentAllInvigilators
  , hundertPercentInMinutes
  , invigilatorsWithMinutesPlanned
  , invigilatorAddMinutes
  , addInvigilatorsPerDay
  , mkInvigilatorsPerDay
  , invigilationsPerPerson
  , addInvigilators
  , minutesForReserve
  )
where

import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                )
import           Data.List                      ( (\\)
                                                , elemIndex
                                                , nub
                                                , partition
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Time.Calendar
import           Data.Time.Format               ( defaultTimeLocale
                                                , parseTimeM
                                                )
import           GHC.Exts                       ( groupWith )

import           Plexams.Query                  ( examDaysPerLecturer )
import           Plexams.Types

allSumInvigilation :: Plan -> Integer
allSumInvigilation plan =
  let Invigilations e r o m l = mkInvigilations plan in e + r + o + m + l

minutesForReserve :: Integer
minutesForReserve = 60

mkInvigilations :: Plan -> Invigilations
mkInvigilations plan
  = let
      sumExams =
        sum
          $ concatMap
              ( map snd
              . removeDoubleRooms
              . concatMap
                  (\exam ->
                    map
                        (\room ->
                          (roomID room, duration exam + deltaDuration room)
                        )
                      $ rooms exam
                  )
              . M.elems
              . examsInSlot
              )
          $ M.elems
          $ slots plan
        -- scheduledExams plan
      removeDoubleRooms :: [(RoomID, Duration)] -> [(RoomID, Duration)]
      removeDoubleRooms = foldr
        (\room@(r, d) rWithD ->
          let (roomsAlreadyIn, otherRooms) = partition ((== r) . fst) rWithD
          in  if null roomsAlreadyIn || snd (head roomsAlreadyIn) < d
                then room : otherRooms
                else rWithD
        )
        []
      sumReserve =
        sum
          $ map
              (\slot' ->
                if M.null (examsInSlot slot') then 0 else minutesForReserve
              )
          $ M.elems
          $ slots plan
      sumOralExams =
        sum $ map invigilatorOralExams $ M.elems $ invigilators plan
      sumMaster = sum $ map invigilatorMaster $ M.elems $ invigilators plan
      sumLivecoding =
        sum $ map invigilatorLiveCoding $ M.elems $ invigilators plan
    in
      Invigilations sumExams sumReserve sumOralExams sumMaster sumLivecoding

addInvigilatorsPerDay :: Plan -> Plan
addInvigilatorsPerDay plan = plan
  { invigilatorsPerDay = M.map (map invigilatorID *** map invigilatorID)
                           $ mkInvigilatorsPerDay plan
  }
                                            -- ( want      , can        )

mkInvigilatorsPerDay :: Plan -> M.Map DayIndex ([Invigilator], [Invigilator])
mkInvigilatorsPerDay =
  foldr
      (\(invigilator', want, day) -> M.alter
        (\invigilators' -> Just $ case invigilators' of
          Nothing ->
            if want then ([invigilator'], []) else ([], [invigilator'])
          Just (wants, cans) -> if want
            then (invigilator' : wants, cans)
            else (wants, invigilator' : cans)
        )
        day
      )
      M.empty
    . concatMap
        (\invigilator' ->
          map (\d -> (invigilator', True, d)) (invigilatorWantDays invigilator')
            ++ map
                 (\d -> (invigilator', False, d))
                 (invigilatorCanDays invigilator')
        )
    . M.elems
    . invigilators

sumPercentInvigilator :: Invigilator -> Integer
sumPercentInvigilator invigilator' =
  let percent = 100 * invigilatorPartTime invigilator'
      overthis =
        if invigilatorOvertimeThisSemester invigilator' == 1.5 then 0.5 else 0
      overlast =
        if invigilatorOvertimeLastSemester invigilator' == 1.5 then 0.5 else 0
      freeSemester = invigilatorFreeSemester invigilator'
      percent'     = (1 + overthis - freeSemester - overlast) * percent
  in  if percent' < 0 then 0 else round percent'

sumPercentAllInvigilators :: Plan -> Integer
sumPercentAllInvigilators =
  sum . map sumPercentInvigilator . M.elems . invigilators

hundertPercentInMinutes :: Plan -> Integer
hundertPercentInMinutes plan =
  let overallSum             = allSumInvigilation plan
      sumPercentInvigilators = sumPercentAllInvigilators plan
  in  overallSum * 100 `div` sumPercentInvigilators

-- removeInvigilatorsWithEnough :: Plan -> Plan
-- removeInvigilatorsWithEnough plan =
--   let hundertPercentInMinutes' = hundertPercentInMinutes plan
--   in plan
--      { invigilators =
--          M.filter
--            (\invigilator' ->
--               invigilatorOralExams invigilator' + invigilatorMaster invigilator' +
--               invigilatorLiveCoding invigilator' <
--               sumPercentInvigilator invigilator' * hundertPercentInMinutes' `div`
--               100) $
--          invigilators plan
--      }
invigilatorsWithMinutesPlanned :: Plan -> M.Map PersonID Integer
invigilatorsWithMinutesPlanned plan
  = let
      reserves :: [(PersonID, Integer)]
      reserves =
        mapMaybe
            ( fmap
                (\invigilator' ->
                  (invigilatorID invigilator', minutesForReserve)
                )
            . reserveInvigilator
            )
          $ M.elems
          $ slots plan
      examsDurations :: [(PersonID, Integer)]
      examsDurations =
        concatMap
            ( concatMap
                (\exam' ->
                  concatMap
                      (\room' ->
                        maybe
                            []
                            (\invigilator' ->
                              [ ( invigilatorID invigilator'
                                , deltaDuration room' + duration exam'
                                )
                              ]
                            )
                          $ invigilator room'
                      )
                    $ rooms exam'
                )
            . M.elems
            . examsInSlot
            )
          $ slots plan
    in
      M.fromList
      $  map
           (\groupedInvigilator ->
             (fst $ head groupedInvigilator, sum $ map snd groupedInvigilator)
           )
      $  groupWith fst
      $  reserves
      ++ examsDurations

invigilatorAddMinutes :: Plan -> Plan
invigilatorAddMinutes plan
  = let
      invigilatorsWithMinutesPlanned' = invigilatorsWithMinutesPlanned plan
      hundertPercentInMinutes'        = hundertPercentInMinutes plan
      invigilatorsWithMinutes =
        M.map
            (\invigilator' -> invigilator'
              { invigilatorMinutesTodo     =
                ((sumPercentInvigilator invigilator' * hundertPercentInMinutes')
                `div` 100
                )
                - invigilatorOralExams invigilator'
                - invigilatorMaster invigilator'
                - invigilatorLiveCoding invigilator'
              , invigilatorsMinutesPlanned = M.findWithDefault
                                               0
                                               (invigilatorID invigilator')
                                               invigilatorsWithMinutesPlanned'
              }
            )
          $ invigilators plan
    in
      plan { invigilators = invigilatorsWithMinutes }
      -- plan' = removeInvigilatorsWithEnough plan

invigilatorsPlanned :: Plan -> [PersonID]
invigilatorsPlanned plan =
  let reserveInvigilators =
        mapMaybe (fmap invigilatorID . reserveInvigilator) $ M.elems $ slots
          plan
      examInvigilators =
        catMaybes
          $ concatMap (map (fmap invigilatorID . invigilator) . rooms)
          $ concatMap (M.elems . examsInSlot)
          $ M.elems
          $ slots plan
  in  nub (reserveInvigilators ++ examInvigilators)

invigilationsPerPerson :: Plan -> M.Map PersonID [Invigilation]
invigilationsPerPerson plan
  = let
      invigilatorsPlanned' = invigilatorsPlanned plan
      mkInvigilations' invigilatorID' = mkReserveInvigilations invigilatorID'
        ++ mkExamInvigilations invigilatorID'
      mkReserveInvigilations invigilatorID' =
        map
            (\((d, s), _) -> Invigilation
              { invigilationInvigilatorID = invigilatorID'
              , invigilationDay           = d
              , invigilationSlot          = s
              , invigilationRoom          = Nothing
              , invigilationDuration      = minutesForReserve
              }
            )
          $ filter
              ( (== Just invigilatorID')
              . fmap invigilatorID
              . reserveInvigilator
              . snd
              )
          $ M.toList
          $ slots plan
      mkExamInvigilations invigilatorID' =
        map
            (\((d, s), ((roomID', duration'), _)) -> Invigilation
              { invigilationInvigilatorID = invigilatorID'
              , invigilationDay           = d
              , invigilationSlot          = s
              , invigilationRoom          = Just roomID'
              , invigilationDuration      = duration'
              }
            )
          $ filter ((== Just invigilatorID') . fmap invigilatorID . snd . snd)
          $ concatMap
              (\(idx, slot') ->
                concatMap
                    (\e ->
                      map
                          (\r ->
                            ( idx
                            , ( (roomID r, duration e + deltaDuration r)
                              , invigilator r
                              )
                            )
                          )
                        $ rooms e
                    )
                  $ M.elems
                  $ examsInSlot slot'
              )
          $ M.toList
          $ slots plan
    in
      M.fromList $ map
        (\invigilatorID' -> (invigilatorID', mkInvigilations' invigilatorID'))
        invigilatorsPlanned'

addInvigilators :: [Invigilator] -> Plan -> Plan
addInvigilators invigilatorList plan =
  let invigilators' = mkInvigilators invigilatorList plan
  in  invigilatorAddMinutes $ plan { invigilators = invigilators' }

mkInvigilators :: [Invigilator] -> Plan -> Invigilators
mkInvigilators invigilatorList plan
  = let
      makeDay :: Text -> Day
      makeDay str = fromMaybe
        (error $ unpack $ "cannot parse date: " `append` str)
        (parseTimeM True defaultTimeLocale "%d.%m.%y" (unpack str))
      examDaysPerLecturer' = examDaysPerLecturer plan
      allDays              = [0 .. maxDayIndex plan]
      examDays' invigilator' =
        M.findWithDefault [] (invigilatorID invigilator') examDaysPerLecturer'
      excludedDays' invigilator' =
        nub
          $  concatMap
               snd
               (filter ((== invigilatorID invigilator') . fst)
                       (noInvigilationDays $ constraints plan)
               )
          ++ mapMaybe
               (flip elemIndex (examDays $ semesterConfig plan) . makeDay)
               (invigilatorExcludedDates invigilator')
      wantDays invigilator' =
        examDays' invigilator' \\ excludedDays' invigilator'
      canDays invigilator' = if length (wantDays invigilator') >= 3
        then []
        else (allDays \\ wantDays invigilator') \\ excludedDays' invigilator'
      addDays' invigilator' = invigilator'
        { invigilatorExamDays     = examDays' invigilator'
        , invigilatorExcludedDays = excludedDays' invigilator'
        , invigilatorWantDays     = wantDays invigilator'
        , invigilatorCanDays      = canDays invigilator'
        }
      personIsInvigilator person =
        not (personIsLBA person)
          && (personFK person == "FK07")
          && ("Prof." `Text.isPrefixOf` personFullName person)
          && (personID person `notElem` noInvigilations (constraints plan))
      addInfoOrCreate :: Person -> Maybe Invigilator -> Maybe Invigilator
      addInfoOrCreate person' Nothing = Just $ addDays' Invigilator
        { invigilatorHasConstraints       = False
        , invigilatorExcludedDays         = []
        , invigilatorExamDays             = []
        , invigilatorWantDays             = []
        , invigilatorCanDays              = allDays
        , invigilatorInvigilationDays     = []
        , invigilatorPerson               = Just person'
        , invigilatorMinutesTodo          = 0
        , invigilatorsMinutesPlanned      = 0
        , invigilatorName                 = personShortName person'
        , invigilatorID                   = personID person'
        , invigilatorExcludedDates        = []
        , invigilatorPartTime             = 1.0
        , invigilatorFreeSemester         = 0.0
        , invigilatorOvertimeThisSemester = 0.0
        , invigilatorOvertimeLastSemester = 0.0
        , invigilatorOralExams            = 0
        , invigilatorMaster               = 0
        , invigilatorLiveCoding           = 0
        }
      addInfoOrCreate person' (Just invigilator') =
        Just $ invigilator' { invigilatorPerson = Just person' }
      addPersonsAndMissingInvigilators
        :: [Person] -> Invigilators -> Invigilators
      addPersonsAndMissingInvigilators persons' invigilators' =
        foldr
            (\person invigilators'' -> M.alter (addInfoOrCreate person)
                                               (personID person)
                                               invigilators''
            )
            invigilators'
          $ filter personIsInvigilator persons'
    in
      addPersonsAndMissingInvigilators (M.elems (persons plan))
      $ M.fromList
      $ filter ((`notElem` noInvigilations (constraints plan)) . fst)
      $ map (invigilatorID &&& addDays') invigilatorList
