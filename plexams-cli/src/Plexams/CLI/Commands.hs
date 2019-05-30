{-# LANGUAGE OverloadedStrings #-}

module Plexams.CLI.Commands
  ( runCommand
  )
where

import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSLI
import           Data.List                      ( intercalate )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

import           Plexams.CLI.Types
import           Plexams.Export.HTML
import           Plexams.Export.Markdown
import           Plexams.Export.Misc
import           Plexams.Export.ZPA
import qualified Plexams.Generators.Invigilation
import qualified Plexams.Generators.Rooms
import           Plexams.Generators.Schedule
import           Plexams.Query
import           Plexams.Statistics
import           Plexams.Types
import qualified Plexams.Validation            as P
                                                ( validate
                                                , validateZPAExport
                                                )

runCommand :: Command -> (Config -> Plan -> IO ())
runCommand Markdown              = markdown
runCommand HTML{}                = html
runCommand Statistics{}          = stats
runCommand Validate{}            = validate
runCommand Query{}               = query
runCommand Export{}              = export
runCommand PrintConfig           = printConfig
runCommand Generate{}            = generate
runCommand GenerateRooms         = generateRooms
runCommand GenerateInvigilations = generateInvigilations
runCommand _                     = error "unsupported command"

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
  maybe (putStrLn output) (`writeFile` output) $ outfile config

stdoutOrFileBS :: Config -> BSLI.ByteString -> IO ()
stdoutOrFileBS config output =
  maybe (print output) (`BSL.writeFile` output) $ outfile config

markdown :: Config -> Plan -> IO ()
markdown config = stdoutOrFile config . planToMD

html :: Config -> Plan -> IO ()
html config = stdoutOrFile config
  . planToHTMLTable (showConflictsAncodes $ optCommand config)

stats :: Config -> Plan -> IO ()
stats config plan =
  let initialStats = case optCommand config of
        Statistics True -> initialPlanStatistics plan
        _               -> ""
      currentStats = planStatistics plan
  in  stdoutOrFile config $ initialStats ++ currentStats

validate :: Config -> Plan -> IO ()
validate config = stdoutOrFile config . validate'
 where
  validateWhat' = case optCommand config of
    Validate sources schedule rooms' invigilations -> concat
      [ [ ValidateSources | sources ]
      , [ ValidateSchedule | schedule ]
      , [ ValidateRooms | rooms' ]
      , [ ValidateInvigilation | invigilations ]
      ]
    _ -> []
  validate' plan =
    let (ok, msgs) = P.validate validateWhat' plan
    in  "\n# " ++ show ok ++ "\n\n" ++ intercalate
          "\n\n"
          (map (Text.unpack . validationMessage) msgs)

query :: Config -> Plan -> IO ()
query config plan = stdoutOrFile config $ intercalate "\n" $ query'
  (optCommand config)
 where
  query' (Query (ByAncode   a) _ m) = map (show' m) $ queryByAnCode a plan
  query' (Query (ByName     n) _ m) = map (show' m) $ queryByName n plan
  query' (Query (ByLecturer l) _ m) = map (show' m) $ queryByLecturer l plan
  query' (Query (ByGroup    g) u m) = map (show' m) $ queryByGroup g u plan
  query' (Query (ByRegisteredGroup g) u m) =
    map (show' m) $ queryByRegisteredGroup g u plan
  query' (Query (BySlot        s) _ _) = map show $ querySlot s plan
  query' (Query (StudentByName s) _ _) = map show $ queryStudentByName s plan
  query' _                             = []
  show' :: Bool -> Exam -> String
  show' False exam = show exam
  show' True exam =
    showMinimal exam ++ ": " ++ showSlot (semesterConfig plan) (slot exam)

export :: Config -> Plan -> IO ()
export config plan = case optCommand config of
  Export ZPA -> do
    stdoutOrFile config $ planToZPA plan
    case outfile config of
      Nothing -> return ()
      Just fp -> do
        (valRes, msgs) <- P.validateZPAExport fp plan
        Text.putStrLn $ Text.intercalate "\n\n" (map validationMessage msgs)
        print valRes
  Export Handicaps       -> stdoutOrFile config $ exportHandicaps plan
  Export PlanForStudents -> stdoutOrFileBS config $ planForStudents plan
  Export StudentsForZPA  -> stdoutOrFile config $ studentRegsToZPA plan
    -- TODO: Validate
  Export Exports ->
    stdoutOrFile config $ intercalate "\n" $ map show $ findExportedExams plan
  _ -> error "unsupported command"

printConfig :: Config -> Plan -> IO ()
printConfig config = stdoutOrFile config . semesterConfigAsString

generate :: Config -> Plan -> IO ()
generate config plan = stdoutOrFile config $ generate' (optCommand config)
 where
  generate' (Generate True) =
    ("# Slots for exams with same name --- generated\n" ++)
      $ exportAddExamToSlots
      $ snd
      $ scheduleExamsWithSameName plan
  generate' _ = error "unsupported command"

generateRooms :: Config -> Plan -> IO ()
generateRooms config plan = stdoutOrFile config generateRooms'
 where
  generateRooms' =
    ("# Rooms for exams --- generated\n" ++)
      $ exportAddRoomToExams
      $ Plexams.Generators.Rooms.generateRooms plan

generateInvigilations :: Config -> Plan -> IO ()
generateInvigilations config plan = stdoutOrFile config generateInvigilations'
 where
  generateInvigilations' =
    ("# Invigilations for exams --- generated\n" ++)
      $ exportAddInvigilatorToRoomOrSlot
      $ Plexams.Generators.Invigilation.generateInvigilations plan
