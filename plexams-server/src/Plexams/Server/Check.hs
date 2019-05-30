-- {-# LANGUAGE DeriveGeneric #-}

module Plexams.Server.Check where

-- import Data.Aeson
-- import GHC.Generics
-- import Plexams.Query
-- import Plexams.Types

-- data CheckError
--   = Ok
--   | FixedSlotError String
--   | OnOneOfTheseDaysError String
--   | NoOnSameDayError String
--   | FileNotFound String
--   deriving (Eq, Ord, Generic)

-- instance ToJSON CheckError

-- checkConstraints :: Plan -> AddExamToSlot -> CheckError
-- checkConstraints plan exam =
--   case fixedSlotCheck of
--     Ok ->
--       case onOneofTheseDayCheck of
--         Ok ->
--           case notOnSameDayCheck of
--             Ok -> Ok
--             _ -> notOnSameDayCheck
--         _ -> onOneofTheseDayCheck
--     _ -> fixedSlotCheck
--   where
--     fixedSlotCheck = checkFixedSlot (fixedSlot constraints') exam
--     onOneofTheseDayCheck =
--       checkOnOneOfTheseDays (onOneOfTheseDays constraints') exam
--     notOnSameDayCheck = checkNotOnSameDay (notOnSameDay constraints') plan exam
--     constraints' = constraints plan

-- checkFixedSlot :: [(Integer, (Int, Int))] -> AddExamToSlot -> CheckError
-- checkFixedSlot fixedSlots exam =
--   if ([] == entry')
--     then Ok
--     else if (entry == exam)
--            then Ok
--            else FixedSlotError $
--                 "Exam is bound to the fixed Slot: (Day: " ++
--                 (show $ planManipDay entry) ++
--                 " Slot: " ++ (show $ planManipSlot entry) ++ ")."
--   where
--     entry =
--       AddExamToSlot
--       { planManipAnCode = fst $ head entry'
--       , planManipDay = fst $ snd $ head entry'
--       , planManipSlot = snd $ snd $ head entry'
--       }
--     entry' = filter (\(x, _) -> x == planManipAnCode exam) fixedSlots

-- checkOnOneOfTheseDays :: [(Ancode, [Int])] -> AddExamToSlot -> CheckError
-- checkOnOneOfTheseDays onOneOfTheseDays' exam =
--   if (length entry' == 0)
--     then Ok
--     else if (allowed)
--            then Ok
--            else OnOneOfTheseDaysError $
--                 "Exam is bound to the following days: " ++
--                 (show $ snd $ head entry')
--   where
--     allowed =
--       elem (toInteger $ planManipDay exam) (map toInteger $ snd $ head entry')
--     entry' = filter (\(x, _) -> x == planManipAnCode exam) onOneOfTheseDays'

-- checkNotOnSameDay :: [[Ancode]] -> Plan -> AddExamToSlot -> CheckError
-- checkNotOnSameDay notOnSameDay' plan exam =
--   if (entries == [])
--     then Ok
--     else if (allowed)
--            then Ok
--            else NoOnSameDayError $
--                 "Exam must not be on the same day as : " ++ (show entries)
--   where
--     allowed = not $ any (\x -> elem x anCodesOnDay) (concat entries)
--     anCodesOnDay = map (anCode) $ queryDay (planManipDay exam) plan
--     entries :: [[Ancode]]
--     entries = filter (elem (planManipAnCode exam)) notOnSameDay'
