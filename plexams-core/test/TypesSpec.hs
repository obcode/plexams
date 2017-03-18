module TypesSpec (spec) where

import           Data.Time.Calendar
import           Plexams.Types
import           Test.Hspec

spec :: Spec
spec =
    describe "The Plexams Types" $
        it "should have 10 real exam days" $
            length (filter isRealExamDay $ examDays emptyPlan)
                `shouldBe` 10

semesterConfig = SemesterConfig
    { semester = "Sommersemester 2017"
    , firstDay = fromGregorian 2017 7 10
    , lastDay = fromGregorian 2017 7 21
    , slotsPerDay = ["08:30", "10:30", "12:30", "14:30", "16:30", "18:30"]
    }

emptyPlan = makeEmptyPlan semesterConfig

