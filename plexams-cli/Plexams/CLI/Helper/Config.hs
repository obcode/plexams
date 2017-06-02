module Plexams.CLI.Helper.Config
  ( configmain
  ) where

import           Data.Semigroup           ((<>))
import           Options.Applicative
import           Plexams.CLI.Helper.Types

configP :: Parser Config
configP = Config
    <$> hsubparser
          ( command "registrations" (info (pure PrepareRegistrations)
                            (progDesc "prepare a registration file"))
         <> command "overlaps" (info (pure PrepareOverlaps)
                            (progDesc "prepare a overlaps file"))
         <> command "students" (info (pure PrepareStudents)
                            (progDesc "prepare a students file"))
          )
    <*> strOption
        ( long "group"
       <> short 'g'
       <> metavar "GROUP"
       <> help "student group (one of IB, IC, IF, IG, IN, IS, GO)"
        )
    <*> strOption
        ( long "infile"
       <> short 'i'
       <> metavar "INFILE"
       <> help "input from file"
        )
    <*> optional ( strOption
         ( long "outfile"
        <> short 'o'
        <> metavar "OUTFILE"
        <> help "write to file (instead of stdout)"
         )
        )

configmain :: (Config -> IO ()) -> IO ()
configmain main' = main' =<< execParser opts
  where
    opts = info (configP <**> helper)
      ( fullDesc
     <> progDesc "Tool for preparing input files for plexams"
     <> header "plexams-helper"
      )
