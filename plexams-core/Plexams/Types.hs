{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Plexams.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.List           (partition)
import           GHC.Generics

data DegreeProgram = IB | IC | IF | GO | IG | IN | IS
    deriving (Eq, Show, Read)

bachelor, master :: [DegreeProgram]
bachelor = [ IB , IC , IF , GO ]
master = [ IG , IN , IS]

data Exam = Exam
    { anCode             :: Integer
    , name               :: String
    , duration           :: Integer
    , reExam             :: Bool
    , lecturer           :: String
    , groups             :: Groups
    , date               :: String
    , time               :: String
    , day                :: Int -- ^ starting with 0
    , slot               :: Int -- ^ starting with 0
    , fk                 :: String
    , rooms              :: [Room]
    , reserveInvigilator :: String
    }
    deriving (Eq, Generic, Show, Read)

studsSum :: Exam -> Int
studsSum = groupSum . groups


instance ToJSON Exam where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Exam

data Room =
  Room { number               :: String
       , maxSeats             :: Int
       , invigilator          :: String
       , reserveRoom          :: Bool
       , handicapCompensation :: Bool
       , deltaDuration        :: Integer
       }
    deriving (Eq, Generic, Show, Read)

type BookableRooms = [(Room, [(Int, Int)])]

instance ToJSON Room where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Room

data Groups =
  Groups { gO :: Int
         , iB :: Int
         , iC :: Int
         , iF :: Int
         , iG :: Int
         , iN :: Int
         , iS :: Int
         }
    deriving (Eq, Generic, Show, Read)

groupSum (Groups gO iB iC iF iG iN iS) =
                gO + iB + iC + iF + iG + iN + iS

grps :: Groups -> [String]
grps (Groups gO iB iC iF iG iN iS) =
        let gOL = if gO == 0 then "" else ("GO(" ++ show gO ++ ")")
            iBL = if iB == 0 then "" else ("IB(" ++ show iB ++ ")")
            iCL = if iC == 0 then "" else ("IC(" ++ show iC ++ ")")
            iFL = if iF == 0 then "" else ("IF(" ++ show iF ++ ")")
            iGL = if iG == 0 then "" else ("IG(" ++ show iG ++ ")")
            iNL = if iN == 0 then "" else ("IN(" ++ show iN ++ ")")
            iSL = if iS == 0 then "" else ("IS(" ++ show iS ++ ")")
        in filter (not . null) [gOL, iBL, iCL, iFL, iGL, iNL, iSL]

instance ToJSON Groups where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Groups

data Constraints = Constraints
    { sameSlots :: [[Integer]] -- ancodes, Prüfungen die zusammen geschrieben werden müssen
    , examsWithFixedSlot :: [(Integer -- ^ ancode, Prüfungen die nicht verschoben werden dürfen
                              , Int     -- ^ day
                              , Int)]   -- ^ slot
    , bookableRooms :: BookableRooms -- wann welcher Raum gebucht werden kann
    , useRoomTogether :: [([Integer], [String])]-- Raumbezeichnung
    , examsWithHandicapCompensation :: [Integer]
    }
  deriving (Show)

defaultConstraints = Constraints
  { sameSlots = []
  , examsWithFixedSlot = []
  , bookableRooms = defaultBookableRooms
  , useRoomTogether = []
  , examsWithHandicapCompensation = []
  }

defaultRoom = Room "" 0 "" False False 0
allSlots = [ (d, s) | d <- [0..9], s <- [0..5] ]

r0006 = defaultRoom { number = "R0.006", maxSeats = 27}
r0007 = defaultRoom { number = "R0.007", maxSeats = 24}
r0009 = defaultRoom { number = "R0.009", maxSeats = 25}
r0010 = defaultRoom { number = "R0.010", maxSeats = 23}
r0011 = defaultRoom { number = "R0.011", maxSeats = 25}
r0012 = defaultRoom { number = "R0.012", maxSeats = 28}
r1006 = defaultRoom { number = "R1.006", maxSeats = 30}
r1007 = defaultRoom { number = "R1.007", maxSeats = 23}
r1008 = defaultRoom { number = "R1.008", maxSeats = 26}
r1046 = defaultRoom { number = "R1.046", maxSeats = 57}
r1049 = defaultRoom { number = "R1.049", maxSeats = 59}
r2007 = defaultRoom { number = "R2.007", maxSeats = 27}
r3014 = defaultRoom { number = "R3.014", maxSeats =  6, handicapCompensation = True}
r3015 = defaultRoom { number = "R3.015", maxSeats =  6, handicapCompensation = True}
r3016 = defaultRoom { number = "R3.016", maxSeats =  8, handicapCompensation = True}
r3017 = defaultRoom { number = "R3.017", maxSeats = 12, handicapCompensation = True}
t0020 = defaultRoom { number = "T0.020", maxSeats = 40}

defaultBookableRooms :: BookableRooms
defaultBookableRooms =
  [ (r0006, allSlots)
  , (r0007, allSlots)
  , (r0009, allSlots)
  , (r0010, allSlots)
  , (r0011, allSlots)
  , (r0012, allSlots)
  , (r1006, allSlots)
  , (r1007, allSlots)
  , (r1008, allSlots)
  , (r1046, [])
  , (r1049, [])
  , (r2007, allSlots)
  , (r3014, allSlots)
  , (r3015, allSlots)
  , (r3016, allSlots)
  , (r3017, allSlots)
  , (t0020, [])
  ]

data Invigilation = Invigilation
  { invigilators       :: [Invigilator]
  , examinations       :: [Exam]
  , invigConstraints   :: Constraints
  , sumDurationExams   :: Integer
  , sumDurationReserve :: Integer
  , sumPercent         :: Double
  }

data Invigilator = Invigilator
  { invigilationTimePlanned :: Integer -- ^ was ist schon eingeplant
  , invigilationTimeTBD     :: Integer -- ^ wieviel muss noch gemacht werden
  , examDays                :: [Int]
  , excludeDays             :: [Int]
  , invigilatorID           :: Int
  , oralExamsContribution   :: Integer -- ^ Anzahl Minuten in mündlichen Prüfungen
  , invigilatorName         :: String
  , excludedDates           :: [String]
  , overtimeLastSemester    :: Double
  , overtimeThisSemester    :: Double
  , partTime                :: Double  -- ^ Teilzeitbeschäftigt 1.0 == Vollzeit
  , freeSemester            :: Double
  , masterContribution      :: Integer -- ^ Anzahl Minuten in Mastergesprächen
  }
    deriving (Eq, Generic, Show, Read)

invigilationTimeOpen :: Invigilator -> Integer
invigilationTimeOpen i = invigilationTimeTBD i - invigilationTimePlanned i

instance ToJSON Invigilator where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Invigilator where
    parseJSON (Object v) = Invigilator 0 0 [] [] <$>
                            v .: "inviligator_id" <*>
                            v .: "oral_exams_contribution" <*>
                            v .: "invigilator" <*>
                            v .: "excluded_dates" <*>
                            v .: "overtime_last_semester" <*>
                            v .: "overtime_this_semester" <*>
                            v .: "part_time" <*>
                            v .: "free_semester" <*>
                            v .: "master_contribution"
    parseJSON _          = empty

realGroupBy _  []           =  []
realGroupBy eq (x:xs)       =  (x:ys) : realGroupBy eq zs
                           where (ys,zs) = partition (eq x) xs

