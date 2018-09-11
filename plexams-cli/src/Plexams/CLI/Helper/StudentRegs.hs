{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Plexams.CLI.Helper.StudentRegs
  ( getStudentsByAncodeJSON
  )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                , empty
                                                )
import           Data.Aeson                     ( ToJSON
                                                , (.=)
                                                , object
                                                , parseJSON
                                                , toJSON
                                                )
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString               as BSI
import qualified Data.ByteString.Lazy          as BSL
import           GHC.Exts                       ( sortWith )

--import Data.ByteString.Lazy.Char8 (unpack)
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Y
import           GHC.Generics

data StudentGroupRegs =
  StudentGroupRegs Text
                   [StudentReg]

students :: StudentGroupRegs -> [StudentReg]
students (StudentGroupRegs _ sr) = sr

instance Y.FromJSON StudentGroupRegs where
  parseJSON (Y.Object v) =
    StudentGroupRegs <$> v Y..: "group" <*> v Y..: "students"
  parseJSON _ = empty

data StudentReg = StudentReg
  { mtknr :: Text
  , familyname :: Text
  , firstname :: Text
  , stg :: Text
  , ancode :: Integer
  } deriving (Generic)

instance Y.FromJSON StudentReg where
  parseJSON (Y.Object v) =
    StudentReg <$> v Y..: "mtknr" <*> v Y..: "familyname" <*> v Y..: "firstname" <*>
    v Y..: "stg" <*>
    v Y..: "ancode"
  parseJSON _ = empty

instance ToJSON StudentReg where
  toJSON sr =
    object
      [ "mtknr" .= mtknr sr
      , "familyname" .= familyname sr
      , "firstname" .= firstname sr
      , "group" .= stg sr
      , "ancode" .= ancode sr
      ]

importStudentRegsFromFile :: FilePath -> IO (Maybe [StudentGroupRegs])
importStudentRegsFromFile = fmap Y.decodeThrow . BSI.readFile

getStudentsByAncode :: FilePath -> Integer -> IO [StudentReg]
getStudentsByAncode fp ancode' = do
  maybeStudentRegsWithGroups <- importStudentRegsFromFile fp
  return $ case maybeStudentRegsWithGroups of
    Nothing -> error $ "cannot parse " ++ fp
    Just srWg ->
      sortWith familyname $ filter ((== ancode') . ancode) $ concatMap
        students
        srWg

getStudentsByAncodeJSON :: FilePath -> FilePath -> Integer -> IO ()
getStudentsByAncodeJSON ofp fp ac = do
  sr <- getStudentsByAncode fp ac
  BSL.writeFile ofp $ encodePretty' config sr
  return ()
 where
  config = defConfig
    { confCompare = keyOrder
                      ["mtknr", "familyname", "firstname", "group", "ancode"]
    }
