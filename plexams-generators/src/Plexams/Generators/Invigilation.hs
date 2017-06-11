module Plexams.Generators.Invigilation where

-- 1. Anzahl Minuten berechnen aus Aufsichten + Zeiten für Beisitz und
--    Auswahlgespräche (Reserveaufsichten zählen Pauschal 60 Minuten)
-- 2. Summe und Anteile Personen berechnen
-- 3. Berechnen wieviele Minuten jeder machen muss
-- 4. Wer bereits durch Beisitz und Auswahlgespräche über seinem Anteil ist,
--    fällt aus der gesamten Berechnung (auch 1.) heraus. Dann neu Berechnung
--    ohne diese

-- Schritt 1: Setze bei allen Prüfungen, bei denen das möglich ist, den Prüfer
-- als Aufsicht. (Nur ein Raum und Tag == want)

-- Ziel: jeder macht +/- 90 Minuten Aufsicht

import           Control.Monad.Writer
import qualified Data.Map             as M
import           Plexams.Invigilation
import           Plexams.Types

generateInvigilations :: Plan -> [AddInvigilatorToRoomOrSlot]
generateInvigilations plan =
  let plan' = invigilatorAddMinutes plan
  in undefined



-- generateSelfInvigilations :: Plan ->
