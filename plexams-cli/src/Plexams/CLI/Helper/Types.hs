module Plexams.CLI.Helper.Types where

data Command =
    PrepareRegistrations -- { pRGroup :: String }
  | PrepareOverlaps --  { pOGroup :: String }
  | PrepareStudents --

data Config = Config
    { optCommand :: Command
    , group      :: String
    , infile     :: FilePath
    , outfile    :: Maybe FilePath
    }
