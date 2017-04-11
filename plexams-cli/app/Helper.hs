module Main where

import           Data.List           (intercalate)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Environment
import           System.IO

data Command = PrepareRegistrations { pRGroup :: String }

data Config = Config
    { optCommand :: Command
    , infile     :: FilePath
    , outfile    :: Maybe FilePath
    }

configP :: Parser Config
configP = Config
    <$> hsubparser
          ( command "regs" (info (prepareRegistrationsOpts)
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

-- TODO: Append to outfile

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
    maybe (putStrLn output) (flip writeFile output) $ outfile config

main' :: Config -> IO ()
main' config = do
    doCommand config

doCommand :: Config -> IO ()
doCommand config@(Config (PrepareRegistrations g) iPath mOPath) = do
    -- read from file and fix newlines
    h <- openFile iPath ReadMode
    hSetEncoding h latin1
    contents <- fmap (map fixNewline) $ hGetContents h
    --
    let examLines =
          map (\e -> "  - ancode: " ++ e!!0 ++ "\n    sum: " ++  e!!4)
            $ filter ((>=5) . length)
            $ map split
            $ tail
            $ lines contents
    stdoutOrFile config $ g ++ ":\n" ++ intercalate "\n" examLines
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



