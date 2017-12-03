module Plexams.CLI.Helper.Commands
  ( runCommand
  ) where

import qualified Data.ByteString          as BSI
import           Data.List                (intercalate, isSuffixOf)
import           Data.Text                (unpack)
import qualified Data.Yaml                as Y
import           GHC.Exts                 (groupWith)
import           Plexams.CLI.Helper.Types
import           System.IO

runCommand :: Config -> IO ()
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
runCommand config@(Config PrepareOverlaps g iPath _) = do
    contents <- getContents' iPath
    let (header' : overlapsLines) = map (split ';') $ lines contents
        header = let h = dropWhile (/='A') $ head header'
                 in h : tail header'
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
    let (header : studentLines) = map (split ';') $ lines contents
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
runCommand config@(Config PrepareAncodes _ iPath _) = do
    contents <- getContents' iPath
    let examLines =
          map ((\e -> "  - ancode:  " ++ head e
                  ++ "\n    codenr:  " ++ e!!1
                  ++ "\n    titel:   " ++ e!!2
                  ++ "\n    stg:     " ++ e!!3
                  ++ "\n    pruefer: " ++ e!!6)
              . split ';')
            $ tail
            $ lines contents
    stdoutOrFile config $ intercalate "\n" (filter (not . null) examLines) ++ "\n"

runCommand config@(Config CheckAncodes _ iPath _) = do
    maybeExams <- importExamsFromYAMLFile iPath
    case maybeExams of
      Nothing -> putStrLn "error: cannot read in exams"
      Just exams -> do
        let examGroups = filter differences $ groupWith ancode exams
        stdoutOrFile config
          $ "goOtherExams:\n"
            ++ intercalate "\n" (map show' examGroups) ++ "\n"

  where
    differences :: [Exam] -> Bool
    differences [] = False
    differences [_] = False
    differences (exam:exams) =
      not $
      all (\e -> {- titel e == titel exam && -} pruefer e == pruefer exam) exams
    show' [] = ""
    show' exams = "  - " ++ show (ancode $ head exams) ++ "\n"
               ++ intercalate "\n" (map showExam exams) ++ "\n"
    showExam (Exam _ _ titel' stg' pruefer') =
      "  # " ++ unpack stg' ++ ": " ++ unpack titel' ++ " (" ++ unpack pruefer' ++ ")"

importExamsFromYAMLFile :: FilePath -> IO (Maybe [Exam])
importExamsFromYAMLFile = fmap Y.decode . BSI.readFile

getContents' :: FilePath -> IO String
getContents' iPath = do
  h <- openFile iPath ReadMode
  hSetEncoding h utf8
  map replaceRwithN <$> hGetContents h
    where replaceRwithN '\r' = '\n'
          replaceRwithN c    = c

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
