module ImportSpec
  ( spec
  )
where

import           Data.Aeson                     ( decode )
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Map                      as M
import           Data.Time.Calendar
import           Plexams.Import.MasterData
import           Plexams.Types
import           Test.Hspec

spec :: Spec
spec = undefined {- do
    describe "The semester config" $ do
        it "should be decoded from a json bytestring" $
            decode semesterConfigJSON
                `shouldBe` Just mySemesterConfig

mySemesterConfig = SemesterConfig
    { semester = "Sommersemester 2017"
    , firstDay = fromGregorian 2017 7 10
    , lastDay = fromGregorian 2017 7 21
    , examDays = [ fromGregorian 2017 7 10
                 , fromGregorian 2017 7 11
                 , fromGregorian 2017 7 12
                 , fromGregorian 2017 7 13
                 , fromGregorian 2017 7 14
                 , fromGregorian 2017 7 17
                 , fromGregorian 2017 7 18
                 , fromGregorian 2017 7 19
                 , fromGregorian 2017 7 20
                 , fromGregorian 2017 7 21
                 ]
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

myPersons = M.fromList [ (1, Person 1 "M, T." "Dr. Thomas Mustermann")
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

myExams = [ Exam 1 "Fach 1" (Person 2 "a" "") 90 [] True False Groups "sp"
        , Exam 2 "Fach 2" (Person 5 "b" "") 60 [] True True  Groups "sp60"
        ]

examsJSON =  "["
 `BS.append` "   {"
 `BS.append` "     \"exam_type\": \"sp\","
 `BS.append` "     \"groups\": [ \"GO\", \"IF\" ],"
 `BS.append` "     \"main_examer\": \"a\","
 `BS.append` "     \"is_repeater_exam\": false,"
 `BS.append` "     \"duration\": 90,"
 `BS.append` "     \"module\": \"Fach 1\","
 `BS.append` "     \"anCode\": 1,"
 `BS.append` "     \"main_examer_id\": 2"
 `BS.append` "   },"
 `BS.append` "   {"
 `BS.append` "     \"exam_type\": \"sp60\","
 `BS.append` "     \"groups\": [ \"IB\", \"IF\" ],"
 `BS.append` "     \"main_examer\": \"b\","
 `BS.append` "     \"is_repeater_exam\": true,"
 `BS.append` "     \"duration\": 60,"
 `BS.append` "     \"module\": \"Fach 2\","
 `BS.append` "     \"anCode\": 2,"
 `BS.append` "     \"main_examer_id\": 5"
 `BS.append` "   }"
 `BS.append` "]"
-}




              