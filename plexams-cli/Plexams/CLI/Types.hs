module Plexams.CLI.Types where

data Command
    = Markdown
    | HTML { showConflictsAncodes :: Maybe [Integer]
           }
    | Statistics { initialStatistics :: Bool
                 }
    | Dot { groupDependencies :: Bool
          }
    | Validate
    | Query { byAncode        :: Maybe Integer
            , byName          :: Maybe String
            , byLecturer      :: Maybe String
            , byGroup         :: Maybe String
            , slot            :: Maybe (Int, Int)
            , onlyUnscheduled :: Bool
            }
    | Export { what :: ExportWhat }
    | PrintConfig
    | Generate { scheduleSameNames :: Bool
               }
    | GenerateRooms
  deriving (Eq)

data Config = Config
    { optCommand      :: Command
    , planManipFile'  :: Maybe FilePath
    , roomsFile       :: Maybe FilePath
    , regsFile        :: Maybe FilePath
    , overlapsFile    :: Maybe FilePath
    , constraintsFile :: Maybe FilePath
    , studentsFile    :: Maybe FilePath
    , outfile         :: Maybe FilePath
    , configfile      :: FilePath
    , novalidation    :: Bool
    }

data ExportWhat = ZPA
                | Handicaps
  deriving (Eq)
