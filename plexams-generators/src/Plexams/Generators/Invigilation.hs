module Plexams.Generators.Invigilation where

-- 1. Anzahl Minuten berechnen aus Aufsichten + Zeiten für Beisitz und
--    Auswahlgespräche (Reserveaufsichten zählen Pauschal 60 Minuten)
-- 2. Summe und Anteile Personen berechnen
-- 3. Berechnen wieviele Minuten jeder machen muss
-- 4. Wer bereits durch Beisitz und Auswahlgespräche über seinem Anteil ist,
--    fällt aus der gesamten Berechnung (auch 1.) heraus. Dann neu Berechnung
--    ohne diese
-- Ziel: jeder macht +/- 90 Minuten Aufsicht
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Plexams.Invigilation
import Plexams.PlanManip
import Plexams.Types

generateInvigilations :: Plan -> [AddInvigilatorToRoomOrSlot]
generateInvigilations plan =
  let plan' =
        setSlotsOnExams $ addInvigilatorsPerDay $ invigilatorAddMinutes plan
  in snd $ runWriter $ generateInvigilations' plan'

generateInvigilations' :: Plan -> Writer [AddInvigilatorToRoomOrSlot] Plan
generateInvigilations'
  -- Schritt 1: Setze bei allen Prüfungen, bei denen das möglich ist, den Prüfer
  -- als Aufsicht. (Nur ein Raum und Tag == want)
 = generateInvigilationsForOwnExams

generateInvigilationsForOwnExams ::
     Plan -> Writer [AddInvigilatorToRoomOrSlot] Plan
generateInvigilationsForOwnExams plan =
  foldM generateInvigilationsForOwnExams' plan $ M.keys $ invigilators plan

addMinutesPlannedToInvigilator :: Plan -> PersonID -> Duration -> Plan
addMinutesPlannedToInvigilator plan invigilatorID' duration' =
  plan
  { invigilators =
      M.alter
        (fmap $ \invigilator' ->
           invigilator'
           { invigilatorsMinutesPlanned =
               invigilatorsMinutesPlanned invigilator' + duration'
           })
        invigilatorID' $
      invigilators plan
  }

invigilationIsPossible :: Invigilator -> Exam -> Bool
invigilationIsPossible i exam =
  (invigilatorMinutesTodo i - invigilatorsMinutesPlanned i > -90) &&
  (fst (fromJust (slot exam)) `elem` invigilatorWantDays i)

generateInvigilationsForOwnExams' ::
     Plan -> PersonID -> Writer [AddInvigilatorToRoomOrSlot] Plan
generateInvigilationsForOwnExams' plan invigilatorID' = do
  let invigilatorsExam =
        filter (any isNothing . map invigilator . rooms) $
        filter ((== 1) . length . rooms) $
        filter ((== invigilatorID') . personID . lecturer) $ scheduledExams plan
      updateExamByAncodeWith' ::
           Plan -> Exam -> Writer [AddInvigilatorToRoomOrSlot] Plan
      updateExamByAncodeWith' plan' exam = do
        let slot' = fromJust $ slot exam
            maybeInvigilator = M.lookup invigilatorID' $ invigilators plan'
        case maybeInvigilator of
          Nothing -> return plan'
          Just invigilator' ->
            if invigilationIsPossible invigilator' exam
              then do
                tell
                  [ AddInvigilatorToRoomOrSlot
                      invigilatorID'
                      slot'
                      (Just $ roomID $ head $ rooms exam)
                  ]
                let plan'' =
                      updateExamByAncodeWith
                        plan'
                        (anCode exam)
                        (\e ->
                           e
                           { rooms =
                               map (\r -> r {invigilator = maybeInvigilator}) $
                               rooms e
                           })
                    plan''' =
                      addMinutesPlannedToInvigilator
                        plan''
                        invigilatorID'
                        (duration exam + head (map deltaDuration $ rooms exam))
                return plan'''
              else return plan'
  if null invigilatorsExam
    then return plan
    else foldM updateExamByAncodeWith' plan invigilatorsExam
-- generateSelfInvigilations :: Plan ->
