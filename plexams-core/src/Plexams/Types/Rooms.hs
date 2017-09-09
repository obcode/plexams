{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.Types.Rooms
  ( Room(..)
  ) where

import           Data.Aeson
import           Data.Text             (unpack)
import           GHC.Generics
import           Plexams.Types.Common
import           Plexams.Types.Persons

data Room = Room
    { roomID               :: RoomID        -- ^ Raum-Nr, z.B. @"R3.014"@
    , maxSeats             :: Integer       -- ^ maximale Anzahl an Prüfungsplätzen
    , deltaDuration        :: Duration      -- ^ falls der Raum für NTA genutzt wird, Anzahl der Minuten
                                            --   die die Prüfung länger
                                            --   dauert
    , invigilator          :: Maybe Invigilator -- ^ Aufsicht
    , reserveRoom          :: Bool          -- ^ @True@, Raum ist eingeplant, wird aber nicht im ZPA
                                            --   veröffentlicht
    , handicapCompensation :: Bool          -- ^ @True@ Raum für NTA
    , seatsPlanned         :: Integer       -- ^ Anzahl der geplanten Studierenden
    }
  deriving (Eq,Generic)

instance FromJSON Room
instance ToJSON Room

instance Show Room where
  show room = roomID room ++ ", "
           ++ show (seatsPlanned room) ++ "/" ++ show (maxSeats room)
           ++ (if reserveRoom room then ", R" else "")
           ++ (if handicapCompensation room
               then ", H (+"++ show (deltaDuration room) ++ "Min)"
               else "")
           ++ maybe " **Aufsicht fehlt**"
                    ((", Aufsicht: "++) . unpack . invigilatorName)
                    (invigilator room)
