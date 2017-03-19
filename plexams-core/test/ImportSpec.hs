{-# LANGUAGE OverloadedStrings #-}
module ImportSpec (spec) where

import           Data.Time.Calendar
import           Plexams.Import
import           Plexams.Types
import           Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Data.Aeson (decode)

spec :: Spec
spec =
    describe "The semester config" $ do
        it "should be made from strings" $
            makeSemesterConfig "Sommersemester 2017" "10.07.2017" "21.07.2017"
                    ["08:30", "10:30", "12:30", "14:30", "16:30", "18:30"]
                `shouldBe` semesterConfig

        it "should be decoded from a json bytestring" $
            decode semesterConfigJSON
                `shouldBe` Just semesterConfig


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
