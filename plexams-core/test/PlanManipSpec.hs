module PlanManipSpec (spec) where

import qualified Data.Map          as M
import           Plexams.PlanManip
import           Plexams.Types
import           Test.Hspec
import           Data.Time.Calendar

spec :: Spec
spec = do
    describe "The plan" $ do
        it "should be generated correctly" $
            makePlan myExams mySemesterConfig Nothing
                `shouldBe` emptyPlan
    describe "The addExamToSlot function" $ do
        it "should add an exam correctly to the initial plan" $
            addExamToSlot 1 2 2 (makePlan myExams mySemesterConfig Nothing)
                `shouldBe` planWith1Scheduled



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

exam1 = Exam 1 "Fach 1" (Person 2 "a" "") 90 [] True False Groups "sp"
exam2 = Exam 2 "Fach 2" (Person 5 "b" "") 60 [] True True  Groups "sp60"

myExams = [ exam1
          , exam2
          ]

emptySlot = Slot
    { examsInSlot = []
    , reserveInvigilator = Nothing
    }

mySlots = M.fromList
    [ ((0,0), emptySlot), ((0,1), emptySlot), ((0,2), emptySlot), ((0,3), emptySlot), ((0,4), emptySlot), ((0,5), emptySlot)
    , ((1,0), emptySlot), ((1,1), emptySlot), ((1,2), emptySlot), ((1,3), emptySlot), ((1,4), emptySlot), ((1,5), emptySlot)
    , ((2,0), emptySlot), ((2,1), emptySlot), ((2,2), emptySlot), ((2,3), emptySlot), ((2,4), emptySlot), ((2,5), emptySlot)
    , ((3,0), emptySlot), ((3,1), emptySlot), ((3,2), emptySlot), ((3,3), emptySlot), ((3,4), emptySlot), ((3,5), emptySlot)
    , ((4,0), emptySlot), ((4,1), emptySlot), ((4,2), emptySlot), ((4,3), emptySlot), ((4,4), emptySlot), ((4,5), emptySlot)
    , ((5,0), emptySlot), ((5,1), emptySlot), ((5,2), emptySlot), ((5,3), emptySlot), ((5,4), emptySlot), ((5,5), emptySlot)
    , ((6,0), emptySlot), ((6,1), emptySlot), ((6,2), emptySlot), ((6,3), emptySlot), ((6,4), emptySlot), ((6,5), emptySlot)
    , ((7,0), emptySlot), ((7,1), emptySlot), ((7,2), emptySlot), ((7,3), emptySlot), ((7,4), emptySlot), ((7,5), emptySlot)
    , ((8,0), emptySlot), ((8,1), emptySlot), ((8,2), emptySlot), ((8,3), emptySlot), ((8,4), emptySlot), ((8,5), emptySlot)
    , ((9,0), emptySlot), ((9,1), emptySlot), ((9,2), emptySlot), ((9,3), emptySlot), ((9,4), emptySlot), ((9,5), emptySlot)
    ]

emptyPlan = Plan
    { semesterConfig = mySemesterConfig
    , slots = mySlots
    , unscheduledExams = myExams
    , persons = M.fromList []
    }

slotWith1Scheduled = Slot
    { examsInSlot = [exam1]
    , reserveInvigilator = Nothing
    }

mySlotsWith1Scheduled = M.fromList
    [ ((0,0), emptySlot), ((0,1), emptySlot), ((0,2), emptySlot), ((0,3), emptySlot), ((0,4), emptySlot), ((0,5), emptySlot)
    , ((1,0), emptySlot), ((1,1), emptySlot), ((1,2), emptySlot), ((1,3), emptySlot), ((1,4), emptySlot), ((1,5), emptySlot)
    , ((2,0), emptySlot), ((2,1), emptySlot),                     ((2,3), emptySlot), ((2,4), emptySlot), ((2,5), emptySlot)
    , ((3,0), emptySlot), ((3,1), emptySlot), ((3,2), emptySlot), ((3,3), emptySlot), ((3,4), emptySlot), ((3,5), emptySlot)
    , ((4,0), emptySlot), ((4,1), emptySlot), ((4,2), emptySlot), ((4,3), emptySlot), ((4,4), emptySlot), ((4,5), emptySlot)
    , ((5,0), emptySlot), ((5,1), emptySlot), ((5,2), emptySlot), ((5,3), emptySlot), ((5,4), emptySlot), ((5,5), emptySlot)
    , ((6,0), emptySlot), ((6,1), emptySlot), ((6,2), emptySlot), ((6,3), emptySlot), ((6,4), emptySlot), ((6,5), emptySlot)
    , ((7,0), emptySlot), ((7,1), emptySlot), ((7,2), emptySlot), ((7,3), emptySlot), ((7,4), emptySlot), ((7,5), emptySlot)
    , ((8,0), emptySlot), ((8,1), emptySlot), ((8,2), emptySlot), ((8,3), emptySlot), ((8,4), emptySlot), ((8,5), emptySlot)
    , ((9,0), emptySlot), ((9,1), emptySlot), ((9,2), emptySlot), ((9,3), emptySlot), ((9,4), emptySlot), ((9,5), emptySlot)
    , ((2,2), slotWith1Scheduled)
    ]

planWith1Scheduled = Plan
    { semesterConfig = mySemesterConfig
    , slots = mySlotsWith1Scheduled
    , unscheduledExams = [exam2]
    , persons = M.fromList []
    }

{-
spec :: Spec
spec =
    describe "The example plan" $ do
        it "should have 10 real exam days" $
            length (filter isRealExamDay $ examDays examplePlan)
                `shouldBe` 10
        it "should have 60 slots" $
            sum (map (length . slotsOfDay) (examDays examplePlan))
                `shouldBe` 60

semesterConfig = SemesterConfig
    { semester = "Sommersemester 2017"
    , firstDay = fromGregorian 2017 7 10
    , lastDay = fromGregorian 2017 7 21
    , slotsPerDay = ["08:30", "10:30", "12:30", "14:30", "16:30", "18:30"]
    }

examplePlan = makeEmptyPlan semesterConfig
-}
