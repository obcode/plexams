module Semester where

import Plexams.Types
import Data.Time.Calendar

semesterConfig = SemesterConfig
    { semester = "Sommersemester 2017"
    , firstDay = fromGregorian 2017 7 10
    , lastDay = fromGregorian 2017 7 21
    , slotsPerDay = ["08:30", "10:30", "12:30", "14:30", "16:30", "18:30"]
    }
