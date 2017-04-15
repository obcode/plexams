module Main where

import           Data.List           (intercalate)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Environment
import           System.IO

data Command =
  PrepareRegistrations { pRGroup :: String }
  | PrepareOverlaps { pOGroup :: String }

data Config = Config
    { optCommand :: Command
    , infile     :: FilePath
    , outfile    :: Maybe FilePath
    }

configP :: Parser Config
configP = Config
    <$> hsubparser
          ( command "regs" (info prepareRegistrationsOpts
                            (progDesc "prepare a registration file"))
         <> command "overlaps" (info prepareOverlapsOpts
                            (progDesc "prepare a overlaps file"))
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

prepareRegistrationsOpts :: Parser Command
prepareRegistrationsOpts = PrepareRegistrations
    <$> strOption
        ( long "group"
       <> short 'g'
       <> metavar "GROUP"
       <> help "student group (one of IB, IC, IF, IG, IN, IS, GO)"
        )


prepareOverlapsOpts :: Parser Command
prepareOverlapsOpts = PrepareOverlaps
    <$> strOption
        ( long "group"
       <> short 'g'
       <> metavar "GROUP"
       <> help "student group (one of IB, IC, IF, IG, IN, IS, GO)"
        )

main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (configP <**> helper)
      ( fullDesc
     <> progDesc "Tool for preparing input files for plexams"
     <> header "plexams-helper"
      )

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
    maybe (putStrLn output) (`appendFile` output) $ outfile config

main' :: Config -> IO ()
main' = doCommand

doCommand :: Config -> IO ()
doCommand config@(Config (PrepareRegistrations g) iPath mOPath) = do
    contents <- getContents' iPath
    let examLines =
          map (\e -> "    - ancode: " ++ head e
                ++ "\n      sum: "    ++  e!!4)
            $ filter ((>=5) . length)
            $ map split
            $ tail
            $ lines contents
    stdoutOrFile config $ "- group: " ++ g
                     ++ "\n  registrations:\n" ++ intercalate "\n" examLines
                     ++ "\n"
doCommand config@(Config (PrepareOverlaps g) iPath mOPath) = do
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
