module Main where

import           Data.List           (intercalate)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Environment
import           System.IO

newtype Command = PrepareRegistrations { pRGroup :: String }

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
    -- read from file and fix newlines
    h <- openFile iPath ReadMode
    hSetEncoding h latin1
    contents <- map fixNewline <$> hGetContents h
    --
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
  where
    fixNewline '\r' = '\n'
    fixNewline x    = x
    split :: String -> [String]
    split [] = []
    split xs =
        let (w, rest) = span (/=';') xs
        in if null rest
           then [w]
           else w : split (dropWhile (==';') rest)
