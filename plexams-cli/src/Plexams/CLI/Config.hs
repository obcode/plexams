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
         <> command "validate"  (info validateOpts
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
         <> command "generate-invigilations"  (info (pure GenerateInvigilations)
                          (progDesc "generate invigilations for the schedule"))
          )
      <*> optional (strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTFILE"
       <> help "output to file instead of stdout"
        ))
    <*> switch
        ( long "no-validation"
       <> help "turn of validation"
        )
    <*> switch
        ( short 'v'
       <> long "verbose"
       <> help "turn on verbosity"
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

validateOpts :: Parser Command
validateOpts = Validate
    <$> switch
        ( long "sources"
       <> help "validation of sources"
        )
    <*> switch
        ( long "schedule"
       <> short 's'
       <> help "validation of schedule"
        )
    <*> switch
        ( long "rooms"
       <> short 'r'
       <> help "validation of rooms"
        )
    <*> switch
        ( long "inviglations"
       <> short 'i'
       <> help "validation of invigilations"
        )

exportOpts :: Parser Command
exportOpts = Export
  <$> (zpaExport <|> handicapsExport <|> planForStudentsExport)

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

planForStudentsExport :: Parser ExportWhat
planForStudentsExport = flag' PlanForStudents
  ( long "planforstudents"
 <> short 'p'
 <> help "export for students")

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
