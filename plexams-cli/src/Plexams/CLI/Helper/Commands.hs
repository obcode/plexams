module Plexams.CLI.Helper.Commands
  ( runCommand
  ) where

import           Data.List                (intercalate, isSuffixOf)
import           Plexams.CLI.Helper.Types
import           System.IO

runCommand :: Config -> IO ()
-- FIXME: für UTF-16 aus Excel
runCommand config@(Config PrepareRegistrations g iPath _) = do
    contents <- getContents' iPath
    let examLines =
          map (\e -> if null $ e!!4 then "" else
                     "  - ancode: " ++ head e
                ++ "\n    sum: "    ++  e!!4)
            $ filter ((>=5) . length)
            $ map (split ';')
            $ tail
            $ lines contents
    stdoutOrFile config $ "- group: " ++ g
                     ++ "\n  registrations:\n"
                     ++ intercalate "\n" (filter (not . null) examLines)
                     ++ "\n"
-- FIXME: für UTF-16 aus Excel
runCommand config@(Config PrepareOverlaps g iPath _) = do
    contents <- getContents' iPath
    let (header : overlapsLines) = map (split ';') $ lines contents
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
    mkOverlapsYaml x = error $ "Cannot mkOverlap " ++ show x
    mkOverlap (ac, noOfStuds) =
                      "        - otherExam: " ++ tail ac ++ "\n"
                   ++ "          noOfStudents: " ++ noOfStuds ++ "\n"
runCommand config@(Config PrepareStudents g iPath _) = do
    contents <- getContents' iPath
    let (header : studentLines) = map (split '\t') $ lines contents
        studentTupels =
          filter ((>2) . length)
          $ map (filter (not . null . snd) . zip header) studentLines
        students = map mkStudentsYaml studentTupels
    stdoutOrFile config $ "# group: " ++ g ++ "\n"
                     ++ intercalate "\n" students
                     ++ "\n"
  where
    get fieldname = snd . head . filter (isSuffixOf fieldname . fst)
    mkStudentsYaml studentTupel =
                      "- mtknr: " ++ get "MTKNR" studentTupel ++ "\n"
                   ++ "  name: " ++ get "NAME" studentTupel ++ "\n"
                   ++ "  ancode: " ++ get "ANCODE" studentTupel ++ "\n"

getContents' :: FilePath -> IO String
getContents' iPath = do
  h <- openFile iPath ReadMode
  hSetEncoding h utf16le
  filter (/='\r') <$> hGetContents h

split :: Char -> String -> [String]
split _ [] = []
split c xs =
    let (w, rest) = span (/=c) xs
    in if null rest
       then [w]
       else w : split c (drop 1 rest)

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
    maybe (putStrLn output) (`writeFile` output) $ outfile config
