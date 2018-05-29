{-# LANGUAGE OverloadedStrings #-}

module Plexams.Import.Registrations
  ( importStudentsWithRegsFromYAMLFile
  , importHandicapsFromYAMLFile
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import qualified Data.ByteString as BSI
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Yaml as Y

import Plexams.Types

--------------------------------------------------------------------------------
-- StudentsRegistrations from YAML file
--------------------------------------------------------------------------------
data ImportStudentRegs =
  ImportStudentRegs Text
                    [ImportStudentReg]

instance Y.FromJSON ImportStudentRegs where
  parseJSON (Y.Object v) =
    ImportStudentRegs <$> v Y..: "group" <*> v Y..: "students"
  parseJSON _ = empty

data ImportStudentReg =
  ImportStudentReg Text
                   Text
                   Text
                   Text
                   Integer

instance Y.FromJSON ImportStudentReg where
  parseJSON (Y.Object v) =
    ImportStudentReg <$> v Y..: "mtknr" <*> v Y..: "familyname" <*>
    v Y..: "firstname" <*>
    v Y..: "stg" <*>
    v Y..: "ancode"
  parseJSON _ = empty

importStudentRegsToStudentsWithRegs :: [ImportStudentReg] -> StudentsWithRegs
importStudentRegsToStudentsWithRegs = foldr insertStudent M.empty
  where
    insertStudent (ImportStudentReg mtkNr familyname' firstname' stg ancode) =
      M.alter
        (Just .
         maybe
           (StudentWithRegs mtkNr familyname' firstname' stg [ancode] Nothing)
           (addAncode ancode))
        mtkNr
    addAncode ancode (StudentWithRegs mktNr familyname' firstname' stg ancodes maybeHandicap) =
      StudentWithRegs
        mktNr
        familyname'
        firstname'
        stg
        (ancode : ancodes)
        maybeHandicap

importStudentGroups :: SemesterConfig -> [ImportStudentRegs] -> StudentsWithRegs
importStudentGroups config importStudentRegs =
  let allRegs = concatMap removeGoOtherExams importStudentRegs
      removeGoOtherExams :: ImportStudentRegs -> [ImportStudentReg]
      removeGoOtherExams (ImportStudentRegs "GO" students') =
        filter ((`notElem` goOtherExams config) . ancode') students'
      removeGoOtherExams (ImportStudentRegs _ students') = students'
      ancode' (ImportStudentReg _ _ _ _ a) = a
  in importStudentRegsToStudentsWithRegs allRegs

importStudentsWithRegsFromYAMLFile ::
     SemesterConfig -> FilePath -> IO (Maybe StudentsWithRegs)
importStudentsWithRegsFromYAMLFile config =
  fmap (fmap (importStudentGroups config) . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- Handicaps from YAML file
--------------------------------------------------------------------------------
importHandicapsFromYAMLFile :: FilePath -> IO (Maybe [Handicap])
importHandicapsFromYAMLFile = fmap Y.decode . BSI.readFile
