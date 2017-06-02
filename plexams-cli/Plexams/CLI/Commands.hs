module Plexams.CLI.Commands
  ( runCommand
  ) where

import           Data.List                   (intercalate)
import           Plexams.CLI.Types
import           Plexams.Export.HTML
import           Plexams.Export.Markdown
import           Plexams.Export.Misc
import           Plexams.Export.ZPA
import qualified Plexams.Generators.Rooms
import           Plexams.Generators.Schedule
import           Plexams.Query
import           Plexams.Statistics
import           Plexams.Types
import qualified Plexams.Validation          as P (validate, validateZPAExport)

runCommand :: Command -> (Config -> Plan -> IO ())
runCommand Markdown      = markdown
runCommand HTML {}       = html
runCommand Statistics {} = stats
runCommand Validate      = validate
runCommand Query {}      = query
runCommand Export {}     = exportZPA
runCommand PrintConfig   = printConfig
runCommand Generate {}   = generate
runCommand GenerateRooms = generateRooms

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
    maybe (putStrLn output) (`writeFile` output) $ outfile config

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
    validate' plan =
      let (ok, msgs) = P.validate plan
      in "\n# " ++ show ok ++ "\n\n" ++ intercalate "\n\n" msgs

query :: Config -> Plan -> IO ()
query config plan = stdoutOrFile config
    $ intercalate "\n" $ map show $ query' (optCommand config)
  where
    query' (Query (Just a) _ _ _ _ _) = queryByAnCode a plan
    query' (Query _ (Just n) _ _ _ _) = queryByName n plan
    query' (Query _ _ (Just l) _ _ _) = queryByLecturer l plan
    query' (Query _ _ _ (Just g) _ u) = queryByGroup g u plan
    query' (Query _ _ _ _ (Just s) _) = querySlot s plan
    query' _                          = []

exportZPA :: Config -> Plan -> IO ()
exportZPA config plan = do
  stdoutOrFile config $ planToZPA plan
  case outfile config of
    Nothing -> return ()
    Just fp -> do
      (valRes, msgs) <- P.validateZPAExport fp plan
      putStrLn $ intercalate "\n\n" msgs
      print valRes

printConfig :: Config -> Plan -> IO ()
printConfig config = stdoutOrFile config . semesterConfigAsString

generate :: Config -> Plan -> IO ()
generate config plan =
  stdoutOrFile config $ generate' (optCommand config)
    where
      generate' (Generate True) =
        ("# Slots for exams with same name --- generated\n"++)
        $ exportAddExamToSlots
        $ snd
        $ scheduleExamsWithSameName plan

generateRooms :: Config -> Plan -> IO ()
generateRooms config plan =
  stdoutOrFile config generateRooms'
    where
      generateRooms'  =
        ("# Rooms for exams --- generated\n"++)
        $ exportAddRoomToExams
        $ snd
        $ Plexams.Generators.Rooms.generateRooms plan
