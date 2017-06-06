{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Plexams.Types
    ( -- * Config
      SemesterConfig(..)
    , Constraints(..)
    , noConstraints
    , setFK10Exam

      -- * Plan
    , Plan(..)
      -- * Slots
    , Slots
    , Slot(..)
    , DayIndex
    , maxDayIndex
    , SlotIndex
    , maxSlotIndex
      -- * Exams
    , Exam(..)
    , allExams
    , scheduledExams
    , isScheduled
    , isUnscheduled
    , withHandicaps
    , Ancode
    , examDateAsString
    , examSlotAsString
    , dateString
    , setSlotsOnExams
    , registrations
      -- * Groups
    , Group(..)
    , parseGroup
    , Degree(..)
    , allDegrees
    , Subgroup(..)
      -- * Persons (lecturer, invigilators) and students
    , Person(..)
    , PersonID
    , Persons
    , Students
    , MtkNr
    , StudentName
      -- * Rooms
    , AvailableRoom(..)
    , AvailableRooms
    , mkAvailableRooms
    , Room(..)
    , seatsMissing
    , RoomID
      -- * Plan manipulation
    , AddExamToSlot(..)
    , AddRoomToExam(..)
      -- * Registrations and overlaps
    , Overlaps(..)
    , Registrations(..)
    , Handicap(..)
    , setHandicapsOnScheduledExams
      -- * Validation
    , ValidationResult(..)
    , validationResult
      -- * ZPA
    , ZPAExam(..)
    , ZPARoom(..)
    ) where

import           Control.Arrow      (second, (***))
import           Data.Char          (digitToInt)
import           Data.List          (intercalate, partition, sortBy, (\\))
import qualified Data.Map           as M
import           Data.Maybe         (isJust, mapMaybe)
import           Data.Monoid        ((<>))
import           Data.Ord           (Down (Down), comparing)
import qualified Data.Set           as S
import           Data.Text          (Text, unpack)
import           Data.Time.Calendar
import           GHC.Exts           (groupWith)
import           GHC.Generics
import           TextShow           (TextShow, showb)

-- fold SemesterConfig
data SemesterConfig = SemesterConfig
    { semester        :: Text     -- ^ Semester
    , firstDay        :: Day      -- ^ Erster Tag des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 10@
    , lastDay         :: Day      -- ^ Letzter Tag  des Prüfungszeitraumes, z.B. @fromGregorian 2017 7 21@
    , examDays        :: [Day]    -- ^ vom ersten bis letzten Tag OHNE Wochenende
    , goSlots         :: [(Int, Int)]
    , slotsPerDay     :: [String] -- ^ Liste von Slots als Zeitstrings in der Form @HH:MM@. Ein Slot ist IMMER 120 Minuten lang
    , initialPlanFile :: FilePath -- ^ Datei in der die Prüfungen für das Semester vom ZPA stehen
    , planManipFile   :: FilePath -- ^ Datei in der die Prüfungen für das Semester vom ZPA stehen
    , availableRooms  :: [AvailableRoom]
    , fk10Exams       :: [[Integer]]
    }
  deriving (Eq, Show, Generic)

data AvailableRoom = AvailableRoom
    { availableRoomName     :: String
    , availableRoomMaxSeats :: Integer
    , availableRoomHandicap :: Bool
    }
  deriving (Eq, Show, Generic)

type AvailableRooms = M.Map (DayIndex, SlotIndex)
                            -- ( Normale Räume (absteigend sortiert nach Größe)
                            -- , Handicap Räume)
                            ([AvailableRoom], [AvailableRoom])

mkAvailableRooms :: Plan -> [AvailableRoom] -> AvailableRooms
mkAvailableRooms _ [] = M.empty
mkAvailableRooms plan rooms =
  let slots' = M.keys $ slots plan
      (handicapCompensationRooms', normalRooms') =
                                          partition availableRoomHandicap rooms
      normalRooms =
        sortBy (comparing (Down . availableRoomMaxSeats)) normalRooms'
      handicapCompensationRooms =
        sortBy (comparing (Down . availableRoomMaxSeats))
                handicapCompensationRooms'
      (handicapCompensationRoomOdd, handicapCompensationRoomEven) =
          (map snd *** map snd)
          $  partition fst
          $ zip (concat $ repeat [True,False]) handicapCompensationRooms
      roomSlots' = maybe M.empty roomSlots $ constraints plan
      -- normalRoomsAlways =
      --  filter (not . (`elem` M.keys roomSlots') . availableRoomName) normalRooms
      allAvailableRooms = M.fromList $ zip slots' $ concat
                        $ repeat [ (normalRooms, handicapCompensationRoomOdd)
                                 , (normalRooms, handicapCompensationRoomEven)]
      removeRoomFromAllOtherSlots :: RoomID -> [(DayIndex, SlotIndex)] -> AvailableRooms
                      -> AvailableRooms
      removeRoomFromAllOtherSlots roomID slots'' allRooms =
        foldr ( -- \s ->
                M.alter (maybe Nothing (\(nr, hr) -> Just
                          ( filter ((/=roomID) . availableRoomName) nr
                          , filter ((/=roomID) . availableRoomName) hr
                          )
                        ))
              ) allRooms
              $ slots' \\ slots''
   in foldr (\(r, sl) allRooms -> removeRoomFromAllOtherSlots r sl allRooms)
            allAvailableRooms $ M.toList roomSlots'
-- endfold

-- fold Plan
data Plan = Plan
    { semesterConfig   :: SemesterConfig
    , slots            :: Slots
    , unscheduledExams :: M.Map Ancode Exam  -- ^ Liste der Prüfungen die noch keinem Slot zugeordnet sind
                                              -- Ancode -> Exam
    , persons          :: Persons
    , constraints      :: Maybe Constraints
    , students         :: Students
    , studentsExams    :: StudentsExams
    , handicaps        :: [Handicap]
    , initialPlan      :: [Exam]
    }
  deriving (Show, Eq)

scheduledExams :: Plan -> [Exam]
scheduledExams plan =
    concatMap (M.elems . examsInSlot . snd) $ M.toList $ slots plan

allExams :: Plan -> [Exam]
allExams plan = let plan' = setSlotsOnExams plan
                in M.elems (unscheduledExams plan') ++ scheduledExams plan'

isScheduledAncode :: Ancode -> Plan -> Bool
isScheduledAncode ancode =
  elem ancode
  . concatMap (M.keys . examsInSlot)
  . M.elems
  . slots

isUnscheduledAncode :: Ancode -> Plan -> Bool
isUnscheduledAncode ancode =
  elem ancode
  . M.keys
  . unscheduledExams

isUnknownExamAncode :: Ancode -> Plan -> Bool
isUnknownExamAncode ancode plan =
     not (isScheduledAncode ancode plan)
  && not (isUnscheduledAncode ancode plan)
-- endfold

-- fold Slots
type DayIndex = Int
type SlotIndex = Int

maxDayIndex :: Plan -> DayIndex
maxDayIndex = (\x->x-1) . length . examDays . semesterConfig

maxSlotIndex :: Plan -> SlotIndex
maxSlotIndex = (\x->x-1) . length . slotsPerDay . semesterConfig

examDateAsString :: Exam -> Plan -> String
examDateAsString exam plan =
  maybe "" (dateString . (examDays (semesterConfig plan)!!) . fst) $ slot exam

examSlotAsString :: Exam -> Plan -> String
examSlotAsString exam plan =
  maybe "" ((slotsPerDay (semesterConfig plan)!!) . snd) $ slot exam

dateString :: Day -> String
dateString day = let (y, m, d) = toGregorian day
                     showWith0 n = let s = show n
                                   in if n < 10 then "0"++s else s
                 in showWith0 d ++ "." ++ showWith0 m ++ "." ++ show y

type Slots = M.Map (DayIndex, SlotIndex) Slot

setSlotsOnExams :: Plan -> Plan
setSlotsOnExams plan = plan
    { slots = M.mapWithKey addSlotKeyToExam $ slots plan
    , unscheduledExams = M.map (\e -> e { slot = Nothing })
                             $ unscheduledExams plan
    }

addSlotKeyToExam :: (DayIndex, SlotIndex) -> Slot -> Slot
addSlotKeyToExam k slot =
    slot { examsInSlot = M.map (addSlotKey k) $ examsInSlot slot }
  where
    addSlotKey k exam = exam { slot = Just k }

data Slot = Slot
    { examsInSlot        :: M.Map Ancode Exam -- Ancode -> Exam
    , reserveInvigilator :: Maybe Integer  -- ^ Reserveaufsicht für die Prüfung
    }
  deriving (Show, Eq)
-- endfold

-- fold Exam
type Ancode = Integer
type Duration = Integer
type ExamType = String

data Exam = Exam
    { anCode                :: Ancode -- ^ Anmeldecode Prüfungsamt
    , name                  :: String  -- ^ Name der Prüfung
    , lecturer              :: Person  -- ^ Prüfer
    , duration              :: Duration -- ^ Dauer der Prüfung in Minuten
    , rooms                 :: [Room]  -- ^ Liste der Räume in denen die Prüfung statt findet
    , plannedByMe           :: Bool    -- ^ @False@ bei Prüfungen, die zwar mit erfasst werden, aber nicht geplant werden
                             --   können
    , reExam                :: Bool    -- ^ @True@ bei einer Wiederholungsklausur
    , groups                :: [Group]  -- ^ Studierendengruppen die an der Prüfung teilnehmen
    , examType              :: ExamType  -- ^ Typ der Prüfung aus ZPA
    , studentsWithHandicaps :: [Handicap]
    , slot                  :: Maybe (Int, Int) -- ^ (Tag, Slot)
    }
  deriving (Eq)

isScheduled :: Exam -> Bool
isScheduled = isJust . slot

isUnscheduled :: Exam -> Bool
isUnscheduled = not . isScheduled

withHandicaps :: Exam -> Bool
withHandicaps = not . null . studentsWithHandicaps

registrations :: Exam -> Integer
registrations = sum . mapMaybe groupRegistrations . groups

setFK10Exam :: [Ancode] -> Exam -> Exam
setFK10Exam ancodes exam =
  exam { plannedByMe = anCode exam `notElem` ancodes }

instance Show Exam where
    show exam = show (anCode exam) ++ ". "
                ++ name exam
                ++ ", " ++ personShortName (lecturer exam)
                ++ (if reExam exam then ", W " else ", E ")
                ++ show (groups exam)
                ++ (if registrations exam > 0
                    then "=" ++ show (registrations exam)
                    else "")
                ++ maybe "" ((", "++) . show) (slot exam)
                ++ (if null $ studentsWithHandicaps exam then ""
                    else "\n        - "
                        ++ intercalate "\n        - "
                            (map show (studentsWithHandicaps exam))
                   )
                ++ (if null $ rooms exam then ""
                   else "\n        - "
                        ++ intercalate "\n        - " (map show (rooms exam))
                   )
-- endfold

-- type BookableRooms = M.Map String (BookableRoom, [(Integer, Integer)])

type RoomID = String

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

seatsMissing :: Exam -> Integer
seatsMissing exam = registrations exam
                  - sum (map seatsPlanned $ rooms exam)

data Group = Group
    { groupDegree        :: Degree
    , groupSemester      :: Maybe Int
    , groupSubgroup      :: Maybe Subgroup
    , groupRegistrations :: Maybe Integer
    }
  deriving (Eq, Ord)

instance Show Group where
    show (Group d mI mS mReg) = show d
      ++ maybe "" show mI
      ++ maybe "" show mS
      ++ maybe "" (("("++) . (++")") . show) mReg

instance TextShow Group where
  showb (Group d mI mS mReg) = showb d
    <> maybe "" showb mI
    <> maybe "" showb mS
    <> maybe "" (("("<>) . (<>")") . showb) mReg

data Degree = IB | IC | IF | GO | IG | IN | IS
  deriving (Show, Eq, Ord, Read, Enum)

instance TextShow Degree where
  showb IB = "IB"
  showb IC = "IC"
  showb IF = "IF"
  showb GO = "GO"
  showb IG = "IG"
  showb IN = "IN"
  showb IS = "IS"

allDegrees :: [Degree]
allDegrees = [IB .. IS]

data Subgroup = A | B | C
  deriving (Show, Eq, Ord)

instance TextShow Subgroup where
  showb A = "A"
  showb B = "B"
  showb C = "C"

instance Read Group where
    readsPrec _ str = [(parseGroup str, "")]

parseGroup :: String -> Group
parseGroup str = Group
    { groupDegree = str2Degree $ take 2 str
    , groupSemester = if length str > 2
                          then Just (digitToInt $ str !! 2)
                          else Nothing
    , groupSubgroup = if length str > 3
                          then Just (char2Subgroup $ str !! 3)
                          else Nothing
    , groupRegistrations = Nothing
    }
  where
    str2Degree str = case str of
                         "IB" -> IB
                         "IC" -> IC
                         "IF" -> IF
                         "GO" -> GO
                         "IG" -> IG
                         "IN" -> IN
                         "IS" -> IS
                         _    -> error $ "unknown group: " ++ str
    char2Subgroup c = case c of
                          'A' -> A
                          'B' -> B
                          'C' -> C
                          _   -> error $ "unknown group: " ++ str

type Persons = M.Map Integer Person
type PersonID = Integer

data Person = Person
    { personID        :: PersonID
    , personShortName :: String
    , personFullName  :: String
    }
  deriving (Eq, Show, Ord)

instance TextShow Person where
  showb (Person id shortName _) =
    showb id <> ". " <> showb shortName

data AddExamToSlot =
    AddExamToSlot
      { planManipAnCode :: Integer
      , planManipDay    :: Int
      , planManipSlot   :: Int
      }

data AddRoomToExam =
    AddRoomToExam
      { addRoomAnCode        :: Integer
      , addRoomRoomName      :: String
      , addRoomSeatsPlanned  :: Integer
      , addRoomDeltaDuration :: Maybe Integer
--      , addRoomReserveRoom :: Bool -- TODO: calculate?
      }
  deriving Show

data Registrations = Registrations
    { regsGroup :: String
    , regs      :: M.Map Integer Integer -- Ancode x Sum
    }
  deriving (Show)

data Constraints = Constraints
  { overlaps                    :: [Overlaps]
  , notOnSameDay                :: [[Ancode]]
  , onOneOfTheseDays            :: [(Ancode, [Int])]
  , fixedSlot                   :: [(Ancode, (Int,Int))]
  , invigilatesExam             :: [(Ancode, PersonID)]
  , impossibleInvigilationSlots :: [(PersonID, [(Int, Int)])]
  , roomSlots                   :: M.Map RoomID [(DayIndex, SlotIndex)]
  }
  deriving (Show, Eq)

noConstraints :: Constraints
noConstraints = Constraints [] [] [] [] [] [] M.empty

data Overlaps = Overlaps
  { olGroup :: Group
  , olOverlaps :: M.Map Integer    -- ancode
                        (M.Map Integer -- otherAncode
                               Integer -- noOfStudents
                         )
  }
  deriving (Show, Eq)

type MtkNr = Integer
type StudentName = Text
type Students = M.Map Ancode (S.Set (MtkNr, StudentName))

type StudentsExams = M.Map MtkNr (StudentName, S.Set Ancode)

data Handicap = Handicap
  { studentname          :: Text
  , mtknr                :: Integer
  , compensation         :: Text
  , deltaDurationPercent :: Integer
  , exams                :: [Ancode]
  , needsRoomAlone       :: Bool
  }
  deriving (Eq)

instance Show Handicap where
  show handicap = unpack (studentname handicap)
                  ++ " (" ++ unpack (compensation handicap) ++ ")"

type Handicaps = M.Map MtkNr Handicap

-- TODO: Brauche ich die Funktion wirklich?
mkHandicaps :: Plan -> [Handicap] -> Handicaps
mkHandicaps plan =
  M.fromList
  -- remove all unknown exams
  . map (\h -> ( mtknr h
               , h {exams = filter (`elem` (map anCode $ allExams plan))
                                   $ exams h}))

updateExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateExamByAncodeWith plan ancode f
  | isScheduledAncode ancode plan =
                                updateScheduledExamByAncodeWith plan ancode f
  | isUnscheduledAncode ancode plan =
                                updateUnscheduledExamByAncodeWith plan ancode f
  | otherwise = plan

updateScheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateScheduledExamByAncodeWith plan ancode f =
  let updatedExamsInSlot = M.alter (fmap f) ancode
                                   $ examsInSlot oldSlotContents
          -- map (second (filter ((==ancode) . anCode) . M.elems . examsInSlot))
      (oldSlot, oldSlotContents) =
        head -- should not fail
        $ filter (elem ancode . map anCode . M.elems . examsInSlot . snd)
        $ M.toList
        $ slots plan
  in plan { slots = M.alter (fmap $ \s -> s {examsInSlot = updatedExamsInSlot})
                            oldSlot $ slots plan}

updateUnscheduledExamByAncodeWith :: Plan -> Ancode -> (Exam -> Exam) -> Plan
updateUnscheduledExamByAncodeWith plan ancode f =
  plan { unscheduledExams = M.alter (fmap f) ancode $ unscheduledExams plan}

setHandicapsOnScheduledExams :: Plan -> Plan
setHandicapsOnScheduledExams plan =
  let handicapsPerAncode =
        map (\aH -> (fst $ head aH, map snd aH))
        $ groupWith fst
        $ concatMap (\h -> map (\a -> (a,h)) $ exams h)
        $ handicaps plan
  in foldr (\(a,hs) p ->
            updateExamByAncodeWith p a (\e -> e {
              studentsWithHandicaps = hs
            })
           )
           plan
           handicapsPerAncode

data ValidationResult = EverythingOk
                      | SoftConstraintsBroken
                      | HardConstraintsBroken
  deriving (Eq, Ord)

instance Show ValidationResult where
  show EverythingOk          = "Validation ok!"
  show SoftConstraintsBroken = ">>> Soft Constraints broken <<<"
  show HardConstraintsBroken = ">>> Hard Constraints broken <<<"

validationResult :: [ValidationResult] -> ValidationResult
validationResult = maximum

-- ZPAExport

data ZPAExam = ZPAExam
    { zpaExamAnCode               :: Integer
    , zpaExamDate                 :: String
    , zpaExamTime                 :: String
    , zpaExamReserveInvigilatorId :: Integer
    , zpaExamRooms                :: [ZPARoom]
    }
  deriving (Generic)

data ZPARoom = ZPARoom
    { zpaRoomNumber               :: String
    , zpaRoomInvigilatorId        :: Integer
    , zpaRoomReserveRoom          :: Bool
    , zpaRoomHandicapCompensation :: Bool
    , zpaRoomDuration             :: Integer
    , zpaRoomNumberStudents       :: Integer
    }
  deriving (Generic)
