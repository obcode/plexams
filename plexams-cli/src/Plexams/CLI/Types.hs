module Plexams.CLI.Types where

data Command
  = Markdown
  | HTML { showConflictsAncodes :: Maybe [Integer] }
  | Statistics { initialStatistics :: Bool }
  | Dot { groupDependencies :: Bool }
  | Validate { validateSources :: Bool
             , validateSchedule :: Bool
             , validateRooms :: Bool
             , validateInvigilation :: Bool }
  | Query { bywhat :: QueryWhat
          , onlyUnscheduled :: Bool }
  | Export { what :: ExportWhat }
  | PrintConfig
  | Generate { scheduleSameNames :: Bool }
  | GenerateRooms
  | GenerateInvigilations
  deriving (Eq)

data Config = Config
  { optCommand :: Command
  , outfile :: Maybe FilePath
  , novalidation :: Bool
  , verbose :: Bool
  }

data QueryWhat
  = ByAncode Integer
  | ByName String
  | ByLecturer String
  | ByGroup String
  | ByRegisteredGroup String
  | BySlot (Int, Int)
  | StudentByName String
  deriving (Eq)

data ExportWhat
  = ZPA
  | Handicaps
  | PlanForStudents
  | StudentsForZPA
  deriving (Eq)
