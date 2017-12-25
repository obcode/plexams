module TypesSpec
  ( spec
  ) where

import Data.Time.Calendar
import Plexams.Types
import Test.Hspec

spec :: Spec
spec =
  describe "The example plan" $ do
    it "should have 10 real exam days" $
      length (filter isRealExamDay $ examDays examplePlan) `shouldBe` 10
    it "should have 60 slots" $
      sum (map (length . slotsOfDay) (examDays examplePlan)) `shouldBe` 60

semesterConfig =
  SemesterConfig
  { semester = "Sommersemester 2017"
  , firstDay = fromGregorian 2017 7 10
  , lastDay = fromGregorian 2017 7 21
  , slotsPerDay = ["08:30", "10:30", "12:30", "14:30", "16:30", "18:30"]
  }

examplePlan = makeEmptyPlan semesterConfig
