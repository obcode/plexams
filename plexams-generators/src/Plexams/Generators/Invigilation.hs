module Plexams.Generators.Invigilation where

-- 1. Anzahl Minuten berechnen aus Aufsichten + Zeiten für Beisitz und
--    Auswahlgespräche (Reserveaufsichten zählen gleich)
-- 2. Summe und Anteile Personen berechnen
-- 3. Berechnen wieviele Minuten jeder machen muss
-- 4. Wer bereits durch Beisitz und Auswahlgespräche über seinem Anteil ist,
--    fällt aus der gesamten Berechnung (auch 1.) heraus.
-- 4. Tripel für Tage berechnen
--   - wer hat da Prüfung und muss Aufsichten machen
--   - Wer hat noch einen Tag übrig
--   - Wer hat den Tag nicht ausgeschlossen
-- 5. zuordnen wobei die kritischen Tage zuerst berechnet werden müssen
-- Ziel: jeder macht +/- 90 Minuten Aufsicht

import           Plexams.Invigilation
