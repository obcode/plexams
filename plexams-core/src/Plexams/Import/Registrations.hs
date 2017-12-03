{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import.Registrations
    ( importRegistrationsFromYAMLFile
    , importOverlapsFromYAMLFile
    , importStudentsFromYAMLFile
    , importStudentsWithRegsFromYAMLFile
    , importHandicapsFromYAMLFile
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import qualified Data.ByteString     as BSI
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Yaml           as Y
import           Plexams.Types

--------------------------------------------------------------------------------
-- Registrations from YAML file
--------------------------------------------------------------------------------

data ImportRegistrations = ImportRegistrations String [ImportRegistration]

instance Y.FromJSON ImportRegistrations where
  parseJSON (Y.Object v) = ImportRegistrations
                        <$> v Y..: "group"
                        <*> v Y..: "registrations"
  parseJSON _            = empty

data ImportRegistration = ImportRegistration Integer Integer

instance Y.FromJSON ImportRegistration where
    parseJSON (Y.Object v) = ImportRegistration
                          <$> v Y..: "ancode"
                          <*> v Y..: "sum"
    parseJSON _            = empty

iRegsToRegs :: SemesterConfig -> ImportRegistrations -> Registrations
iRegsToRegs config (ImportRegistrations g rs) = Registrations
  { regsGroup = g
  , regs = M.fromList
           $ if g == "GO"
             then filter ((`notElem` goOtherExams config) . fst) regList
             else regList

  }
  where regList = map (\(ImportRegistration a s) -> (a, s)) rs

iRegsLToRegsL :: SemesterConfig -> [ImportRegistrations] -> [Registrations]
iRegsLToRegsL config = map (iRegsToRegs config)

importRegistrationsFromYAMLFile :: SemesterConfig -> FilePath -> IO (Maybe [Registrations])
importRegistrationsFromYAMLFile config =
    fmap (fmap (iRegsLToRegsL config) . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- Overlaps from YAML file
--------------------------------------------------------------------------------

data ImportOverlaps = ImportOverlaps String [ImportOverlapsList]

instance Y.FromJSON ImportOverlaps where
  parseJSON (Y.Object v) = ImportOverlaps
                        <$> v Y..: "group"
                        <*> v Y..: "overlapsList"
  parseJSON _            = empty

data ImportOverlapsList = ImportOverlapsList Integer [ImportOverlap]

instance Y.FromJSON ImportOverlapsList where
  parseJSON (Y.Object v) = ImportOverlapsList
                        <$> v Y..: "ancode"
                        <*> v Y..: "overlaps"
  parseJSON _            = empty

data ImportOverlap = ImportOverlap Integer Integer

instance Y.FromJSON ImportOverlap where
    parseJSON (Y.Object v) = ImportOverlap
                          <$> v Y..: "otherExam"
                          <*> v Y..: "noOfStudents"
    parseJSON _            = empty

iOLToOL :: ImportOverlaps -> Overlaps
iOLToOL (ImportOverlaps g rs) = Overlaps
  { olGroup = read g
  , olOverlaps = M.fromList
        $ map (\(ImportOverlapsList a s) -> (a, M.fromList $ map toTupel s)) rs
  }
  where toTupel (ImportOverlap a s) = (a,s)

iOLToOLList :: [ImportOverlaps] -> [Overlaps]
iOLToOLList = map iOLToOL

importOverlapsFromYAMLFile :: FilePath -> IO (Maybe [Overlaps])
importOverlapsFromYAMLFile =
    fmap (fmap iOLToOLList . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- Students from YAML file
--------------------------------------------------------------------------------

data ImportStudent = ImportStudent Integer Text Integer

instance Y.FromJSON ImportStudent where
  parseJSON (Y.Object v) = ImportStudent
                        <$> v Y..: "mtknr"
                        <*> v Y..: "name"
                        <*> v Y..: "ancode"
  parseJSON _            = empty

importStudentsToStudents :: [ImportStudent] -> Students
importStudentsToStudents = foldr insertStudent M.empty
  where
    insertStudent (ImportStudent mtkNr name' ancode) =
      M.alter (Just . maybe (S.singleton (mtkNr,name'))
                            (S.insert (mtkNr,name'))) ancode

importStudentsFromYAMLFile :: FilePath -> IO (Maybe Students)
importStudentsFromYAMLFile =
    fmap (fmap importStudentsToStudents . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- StudentsRegistrations from YAML file
--------------------------------------------------------------------------------

data ImportStudentRegs = ImportStudentRegs Text [ImportStudentReg]

instance Y.FromJSON ImportStudentRegs where
  parseJSON (Y.Object v) = ImportStudentRegs
                        <$> v Y..: "group"
                        <*> v Y..: "students"
  parseJSON _            = empty

data ImportStudentReg = ImportStudentReg Integer Text Text Integer

instance Y.FromJSON ImportStudentReg where
  parseJSON (Y.Object v) = ImportStudentReg
                        <$> v Y..: "mtknr"
                        <*> v Y..: "name"
                        <*> v Y..: "stg"
                        <*> v Y..: "ancode"
  parseJSON _            = empty

importStudentRegsToStudentsWithRegs :: [ImportStudentReg] -> StudentsWithRegs
importStudentRegsToStudentsWithRegs = foldr insertStudent M.empty
  where
    insertStudent (ImportStudentReg mtkNr name' stg ancode) =
      M.alter (Just . maybe (StudentWithRegs mtkNr name' stg [ancode])
                            (addAncode ancode)) mtkNr
    addAncode ancode (StudentWithRegs mktNr name' stg ancodes) =
        StudentWithRegs mktNr name' stg (ancode : ancodes)

importStudentGroups :: SemesterConfig -> [ImportStudentRegs] -> StudentsWithRegs
importStudentGroups config importStudentRegs =
  let allRegs = concatMap removeGoOtherExams importStudentRegs
      removeGoOtherExams :: ImportStudentRegs -> [ImportStudentReg]
      removeGoOtherExams (ImportStudentRegs "GO" students') =
          filter ((`notElem` goOtherExams config) . ancode') students'
      removeGoOtherExams (ImportStudentRegs _ students') = students'
      ancode' (ImportStudentReg _ _ _ a) = a
  in importStudentRegsToStudentsWithRegs allRegs

importStudentsWithRegsFromYAMLFile ::
  SemesterConfig -> FilePath -> IO (Maybe StudentsWithRegs)
importStudentsWithRegsFromYAMLFile config =
    fmap (fmap (importStudentGroups config) . Y.decode) . BSI.readFile

--------------------------------------------------------------------------------
-- Handicaps from YAML file
--------------------------------------------------------------------------------

importHandicapsFromYAMLFile :: FilePath -> IO (Maybe [Handicap])
importHandicapsFromYAMLFile =
    fmap Y.decode . BSI.readFile
