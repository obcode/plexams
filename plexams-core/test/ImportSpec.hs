{-# LANGUAGE OverloadedStrings #-}
module ImportSpec (spec) where

import           Data.Aeson           (decode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map             as M
import           Data.Time.Calendar
import           Plexams.Import
import           Plexams.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "The semester config" $ do
        it "should be made from strings" $
            makeSemesterConfig "Sommersemester 2017" "10.07.2017" "21.07.2017"
                    ["08:30", "10:30", "12:30", "14:30", "16:30", "18:30"]
                `shouldBe` semesterConfig

        it "should be decoded from a json bytestring" $
            decode semesterConfigJSON
                `shouldBe` Just semesterConfig

    describe "The persons" $ do
        it "should be made from a json bytestring" $
            decodePersonsFromJSON personsJSON
                `shouldBe` Just persons

semesterConfig = SemesterConfig
    { semester = "Sommersemester 2017"
    , firstDay = fromGregorian 2017 7 10
    , lastDay = fromGregorian 2017 7 21
    , slotsPerDay = ["08:30", "10:30", "12:30", "14:30", "16:30", "18:30"]
    }

semesterConfigJSON =
                "{"
    `BS.append` "    \"semester\": \"Sommersemester 2017\","
    `BS.append` "    \"firstDay\": \"10.07.2017\","
    `BS.append` "    \"lastDay\": \"21.07.2017\","
    `BS.append` "    \"slotsPerDay\": ["
    `BS.append` "        \"08:30\", \"10:30\", \"12:30\", \"14:30\", \"16:30\", \"18:30\""
    `BS.append` "    ]"
    `BS.append` "}"

persons = M.fromList [ (1, Person 1 "M, T." "Dr. Thomas Mustermann")
                     , (4, Person 4 "M, J." " Julia Musterfrau")
                     , (5, Person 5 "M, R." "Dr. Ruth Musterfrau")
                     ]

personsJSON =
                "["
    `BS.append` "    {"
    `BS.append` "        \"person_id\": 1,"
    `BS.append` "        \"person_shortname\": \"M, T.\","
    `BS.append` "        \"person_fullname\": \"Dr. Thomas Mustermann\""
    `BS.append` "    },"
    `BS.append` "    {"
    `BS.append` "        \"person_id\": 4,"
    `BS.append` "        \"person_shortname\": \"M, J.\","
    `BS.append` "        \"person_fullname\": \" Julia Musterfrau\""
    `BS.append` "    },"
    `BS.append` "    {"
    `BS.append` "        \"person_id\": 5,"
    `BS.append` "        \"person_shortname\": \"M, R.\","
    `BS.append` "        \"person_fullname\": \"Dr. Ruth Musterfrau\""
    `BS.append` "    }"
    `BS.append` "]"

