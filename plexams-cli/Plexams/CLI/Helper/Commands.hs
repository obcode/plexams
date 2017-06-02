module Plexams.CLI.Helper.Commands
  ( runCommand
  ) where

import           Data.List                (intercalate)
import           Plexams.CLI.Helper.Types
import           System.IO

runCommand :: Config -> IO ()
runCommand config@(Config PrepareRegistrations g iPath mOPath) = do
    contents <- getContents' iPath
    let examLines =
          map (\e -> if null $ e!!4 then "" else
                     "  - ancode: " ++ head e
                ++ "\n    sum: "    ++  e!!4)
            $ filter ((>=5) . length)
            $ map split
            $ tail
            $ lines contents
    stdoutOrFile config $ "- group: " ++ g
                     ++ "\n  registrations:\n"
                     ++ intercalate "\n" (filter (not . null) examLines)
                     ++ "\n"
runCommand config@(Config PrepareOverlaps g iPath mOPath) = do
    contents <- getContents' iPath
    let (header : overlapsLines) = map split $ lines contents
        overlapsTupels =
          filter ((>3) . length)
          $ map (filter (not . null . snd) . zip header) overlapsLines
        overlaps = map mkOverlapsYaml overlapsTupels
    stdoutOrFile config $ "- group: " ++ g
                     ++ "\n  overlapsList:\n"
                     -- ++ show overlaps
                     ++ intercalate "\n" overlaps
                     ++ "\n"
  where
    mkOverlapsYaml (("AnCode", ac):_:_:overlaps) =
                      "    - ancode: " ++ tail ac ++ "\n"
                   ++ "      overlaps:\n"
                   ++ concatMap mkOverlap overlaps
    mkOverlap (ac, noOfStuds) =
                      "        - otherExam: " ++ tail ac ++ "\n"
                   ++ "          noOfStudents: " ++ noOfStuds ++ "\n"

runCommand config@(Config PrepareStudents g iPath mOPath) = do
    contents <- getContents' iPath
    let (header : studentLines) = map split $ lines contents
        studentTupels =
          filter ((>2) . length)
          $ map (filter (not . null . snd) . zip header) studentLines
        students = map mkStudentsYaml studentTupels
    stdoutOrFile config $ "# group: " ++ g ++ "\n"
                     ++ intercalate "\n" students
                     ++ "\n"
  where
    mkStudentsYaml (("MTKNR", mtknr):("ANCODE", ac):_:overlaps) =
                      "- mtknr: " ++ mtknr ++ "\n"
                   ++ "  ancode: " ++ ac ++ "\n"

getContents' :: FilePath -> IO String
getContents' iPath = do
  h <- openFile iPath ReadMode
  hSetEncoding h latin1
  map fixNewline <$> hGetContents h

fixNewline :: Char -> Char
fixNewline '\r' = '\n'
fixNewline x    = x

split :: String -> [String]
split [] = []
split xs =
    let (w, rest) = span (/=';') xs
    in if null rest
       then [w]
       else w : split (drop 1 rest)

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
    maybe (putStrLn output) (`writeFile` output) $ outfile config
