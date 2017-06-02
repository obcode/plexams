module Plexams.CLI.Config
  ( configmain
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Plexams.CLI.Types

config :: Parser Config
config = Config
    <$> hsubparser
          ( command "markdown"    (info (pure Markdown)
                                  (progDesc "the plan as markdown document"))
         <> command "html"      (info htmlOpts
                                 (progDesc "the plan as an HTML table"))
         <> command "stats"     (info statisticsOpts
                                  (progDesc "statistics"))
         <> command "validate"  (info (pure Validate)
                                  (progDesc "validation of current plan"))
         <> command "query"     (info  queryOpts
                                  (progDesc "query plan"))
         <> command "export"    (info  exportOpts
                                  (progDesc "export current plan for ZPA"))
         <> command "config"    (info  (pure PrintConfig)
                                  (progDesc "print the current config"))
         <> command "generate"  (info generateOpts
                                  (progDesc "generate part of the plan"))
         <> command "generate-rooms"  (info (pure GenerateRooms)
                                  (progDesc "generate rooms for the schedule"))
          )
      <*> optional (strOption
        ( long "planManip"
       <> short 'p'
       <> metavar "PLANMANIPFILE"
       <> help "import file containing exam2slot manipulations"
        ))
      <*> optional (strOption
        ( long "rooms"
       -- <> short 'p'
       <> metavar "ROOMSFILE"
       <> help "import file containing room2slot manipulations"
        ))
      <*> optional (strOption
        ( long "registrations"
       <> short 'r'
       <> metavar "REGISTRATIONSFILE"
       <> help "import file containing registrations"
        ))
      <*> optional (strOption
        ( long "overlaps"
       <> short 'l'
       <> metavar "OVERLAPSFILE"
       <> help "import file containing overlaps"
        ))
      <*> optional (strOption
        ( long "constraints"
       <> short 'c'
       <> metavar "CONSTRAINTSFILE"
       <> help "import file containing constraints"
        ))
      <*> optional (strOption
        ( long "students"
       <> short 's'
       <> metavar "STUDENTSFILE"
       <> help "import file containing registrations for each mtknr"
        ))
      <*> optional (strOption
        ( long "handicaps"
       <> metavar "HANDICAPSFILE"
       <> help "import file containing handicap information"
        ))
      <*> optional (strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTFILE"
       <> help "output to file instead of stdout"
        ))
      <*> strOption
        ( long "config"
       <> short 'c'
       <> showDefault
       <> value "plexams.yaml"
       <> metavar "CONFIGFILE"
       <> help "file containing semesterconfig"
        )
    <*> switch
        ( long "no-validation"
       <> help "turn of validation"
        )

htmlOpts :: Parser Command
htmlOpts = HTML
    <$> optional (option auto
        ( long "ancodes"
       <> short 'a'
       <> metavar "LISTOFANCODES"
       <> help "show conflicts for given ancodes"
        ))

queryOpts :: Parser Command
queryOpts = Query
    <$> (   queryByAncode
        <|> queryByName
        <|> queryByLecturer
        <|> queryByGroup
        <|> queryBySlot
        <|> queryStudentByName
        )
    <*> switch
        ( long "unscheduled-only"
       <> short 'u'
       <> help "show only unscheduled"
        )

queryByAncode :: Parser QueryWhat
queryByAncode = ByAncode
      <$> option auto
        ( long "ancode"
       <> short 'a'
       <> metavar "EXAMID"
       <> help "query by exam id"
        )

queryByName :: Parser QueryWhat
queryByName = ByName
      <$> strOption
        ( long "name"
       <> short 'n'
       <> metavar "NAME"
       <> help "query by name (substring of name)"
        )

queryByLecturer :: Parser QueryWhat
queryByLecturer = ByLecturer
    <$> strOption
        ( long "lecturer"
       <> short 'l'
       <> metavar "LECTURERNAME"
       <> help "query by lecturer name (substring of name)"
        )

queryByGroup :: Parser QueryWhat
queryByGroup = ByGroup
    <$> strOption
        ( long "group"
       <> short 'g'
       <> metavar "GROUP"
       <> help "query by group"
        )

queryBySlot :: Parser QueryWhat
queryBySlot = ByGroup
  <$> option auto
      ( long "slot"
     <> short 's'
     <> metavar "(DAYINDEX,SLOTINDEX)"
     <> help "query the slot"
      )

queryStudentByName :: Parser QueryWhat
queryStudentByName = StudentByName
  <$> strOption
      ( long "studentname"
     <> metavar "STUDENTNAME"
     <> help "query by student name (substring of name)"
      )

statisticsOpts :: Parser Command
statisticsOpts = Statistics
    <$> switch
        ( long "initial"
       <> short 'i'
       <> help "statistics for initial plan"
        )

exportOpts :: Parser Command
exportOpts = Export
  <$> (zpaExport <|> handicapsExport)

zpaExport :: Parser ExportWhat
zpaExport = flag' ZPA
  ( long "zpa"
 <> short 'z'
 <> help "export for ZPA")

handicapsExport :: Parser ExportWhat
handicapsExport = flag' Handicaps
  ( long "compensation"
 <> short 'c'
 <> help "export exams with handicap compensation"
  )

generateOpts :: Parser Command
generateOpts = Generate
    <$> switch
        ( long "schedule-same-name"
       <> short 'n'
       <> help ("schedule unscheduled exams with same name "
                ++ " than the ONE already scheduled")
        )

configmain :: (Config -> IO ()) -> IO ()
configmain main' = main' =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Tool for planning exams"
     <> header "plexams"
      )
