module Plexams.CLI.Helper.Commands
  ( runCommand
  )
where

import           Data.List                      ( intercalate
                                                , isSuffixOf
                                                )
import           Data.Text                      ( unpack )
import qualified Data.Yaml                     as Y
import           GHC.Exts                       ( groupWith )
import           System.IO
import           System.Exit                    ( exitFailure )

import           Plexams.CLI.Helper.StudentRegs
import           Plexams.CLI.Helper.Types


runCommand :: Config -> IO ()
runCommand config@(Config PrepareStudentRegs (Just g) iPath _) = do
  contents <- getContents' iPath
  let (header : studentLines) = map (split ';') $ lines contents
      studentTupels           = filter ((> 2) . length)
        $ map (filter (not . null . snd) . zip header) studentLines
      students = map mkStudentsYaml studentTupels
  stdoutOrFile config
    $  "- group: "
    ++ g
    ++ "\n  students:\n"
    ++ intercalate "\n" students
    ++ "\n"
 where
  get fieldname = snd . head . filter (isSuffixOf fieldname . fst)
  getName studentTupel =
    let (family, first) = span (/= ',') $ get "name" studentTupel
    in  (family, dropWhile (== ' ') $ tail first)
  mkStudentsYaml studentTupel =
    "  - mtknr: '"
      ++ get "MTKNR" studentTupel
      ++ "'\n    familyname: "
      ++ fst (getName studentTupel)
      ++ "\n    firstname: "
      ++ snd (getName studentTupel)
      ++ "\n    stg: "
      ++ get "Stg" studentTupel
      ++ "\n    ancode: "
      ++ get "AnCode" studentTupel
      ++ "\n"
runCommand config@(Config PrepareAncodes _ iPath _) = do
  contents <- getContents' iPath
  let examLines =
        map
            ( (\e ->
                "  - ancode:  "
                  ++ head e
                  ++ "\n    titel:   "
                  ++ e
                  !! 2
                  ++ "\n    stg:     "
                  ++ e
                  !! 3
                  ++ "\n    pruefer: "
                  ++ e
                  !! 6
              )
            . split ';'
            )
          $ tail
          $ lines contents
  stdoutOrFile config $ intercalate "\n" (filter (not . null) examLines) ++ "\n"
runCommand config@(Config CheckAncodes _ iPath _) = do
  exams <- importExamsFromYAMLFile iPath
  let examGroups = filter differences $ groupWith ancode exams
  stdoutOrFile config
    $  "goOtherExams:\n"
    ++ intercalate "\n" (map show' examGroups)
    ++ "\n"
 where
  differences :: [Exam] -> Bool
  differences []  = False
  differences [_] = False
  differences (exam : exams) =
    not $ all (\e -> pruefer e == pruefer exam) exams {- titel e == titel exam && -}
  show' [] = ""
  show' exams =
    "  - "
      ++ show (ancode $ head exams)
      ++ "\n"
      ++ intercalate "\n" (map showExam exams)
      ++ "\n"
  showExam (Exam _ titel' stg' pruefer') =
    "  # "
      ++ unpack stg'
      ++ ": "
      ++ unpack titel'
      ++ " ("
      ++ unpack pruefer'
      ++ ")"
runCommand (Config (GetStudsForAncode ac) _ iPath (Just ofp)) =
  getStudentsByAncodeJSON ofp iPath ac
runCommand _ = putStrLn "unknown config"

importExamsFromYAMLFile :: FilePath -> IO [Exam]
importExamsFromYAMLFile fp = do
  res <- Y.decodeFileWithWarnings fp
  case res of
    Left parseEx -> do
      putStrLn $ Y.prettyPrintParseException parseEx
      exitFailure
    Right (warnings, exams') -> do
      mapM_ print warnings
      return exams' -- = fmap Y.decodeThrow . BSI.readFile

getContents' :: FilePath -> IO String
getContents' iPath = do
  h <- openFile iPath ReadMode
  hSetEncoding h utf8
  hGetContents h
  -- where
  --   replaceRwithN '\r' = '\n'
  --   replaceRwithN c = c

split :: Char -> String -> [String]
split _ [] = []
split c xs =
  let (w, rest) = span (/= c) xs
  in  if null rest then [w] else w : split c (drop 1 rest)

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
  maybe (putStrLn output) (`writeFile` output) $ outfile config
