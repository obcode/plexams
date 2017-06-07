{-# LANGUAGE OverloadedStrings #-}
module Plexams.Import.Misc
    ( importZPAExamsFromJSONFile
    ) where

import           Control.Applicative  (empty, (<$>), (<*>))
import           Data.Aeson           (FromJSON, Value (Object), decode,
                                       parseJSON, (.:))
import qualified Data.ByteString.Lazy as BS
import           Plexams.Types

--------------------------------------------------------------------------------
-- ZPAExams from JSON file
--------------------------------------------------------------------------------

instance FromJSON ZPAExam where
    parseJSON (Object v ) = ZPAExam
                         <$> v .: "anCode"
                         <*> v .: "date"
                         <*> v .: "time"
                         <*> v .: "total_number"
                         <*> v .: "reserveInvigilator_id"
                         <*> v .: "rooms"
    parseJSON _          = empty

instance FromJSON ZPARoom where
    parseJSON (Object v ) = ZPARoom
                         <$> v .: "number"
                         <*> v .: "invigilator_id"
                         <*> v .: "reserveRoom"
                         <*> v .: "handicapCompensation"
                         <*> v .: "duration"
                         <*> v .: "numberStudents"
    parseJSON _          = empty

importZPAExamsFromJSONFile :: FilePath -> IO (Maybe [ZPAExam])
importZPAExamsFromJSONFile = fmap decode . BS.readFile
