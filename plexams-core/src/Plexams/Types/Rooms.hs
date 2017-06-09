module Plexams.Types.Rooms
  ( Room(..)
  ) where

import           Plexams.Types.Common

data Room = Room
    { roomID               :: RoomID        -- ^ Raum-Nr, z.B. @"R3.014"@
    , maxSeats             :: Integer       -- ^ maximale Anzahl an Prüfungsplätzen
    , deltaDuration        :: Duration      -- ^ falls der Raum für NTA genutzt wird, Anzahl der Minuten
                                            --   die die Prüfung länger
                                            --   dauert
    , invigilator          :: Maybe Integer -- ^ Aufsicht
    , reserveRoom          :: Bool          -- ^ @True@, Raum ist eingeplant, wird aber nicht im ZPA
                                            --   veröffentlicht
    , handicapCompensation :: Bool          -- ^ @True@ Raum für NTA
    , seatsPlanned         :: Integer       -- ^ Anzahl der geplanten Studierenden
    }
  deriving (Eq)

instance Show Room where
  show room = roomID room ++ ", "
           ++ show (seatsPlanned room) ++ "/" ++ show (maxSeats room)
           ++ (if reserveRoom room then ", R" else "")
           ++ (if handicapCompensation room
               then ", H (+"++ show (deltaDuration room) ++ "Min)"
               else "")
