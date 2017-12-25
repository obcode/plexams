{-# LANGUAGE OverloadedStrings #-}

module Plexams.CLI.Helper.Types where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Text (Text)
import qualified Data.Yaml as Y

data Command
  = PrepareRegistrations -- { pRGroup :: String }
  | PrepareOverlaps --  { pOGroup :: String }
  | PrepareStudents --
  | PrepareStudentRegs
  | PrepareAncodes
  | CheckAncodes

data Config = Config
  { optCommand :: Command
  , group :: String
  , infile :: FilePath
  , outfile :: Maybe FilePath
  }

data Exam = Exam
  { ancode :: Integer
  , codenr :: Integer
  , titel :: Text
  , stg :: Text
  , pruefer :: Text
  } deriving (Show)

instance Y.FromJSON Exam where
  parseJSON (Y.Object v) =
    Exam <$> v Y..: "ancode" <*> v Y..: "codenr" <*> v Y..: "titel" <*>
    v Y..: "stg" <*>
    v Y..: "pruefer"
  parseJSON _ = empty
