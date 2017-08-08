{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Plexams.Server
  ( startApp
  ) where

import           Control.Monad.Except
import           Data.Time.Calendar
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Plexams.CLI.Import
-- import           Plexams.CLI.Config
import           Plexams.CLI.PlanManip
-- import           Plexams.CLI.Types
-- import           Plexams.Import.MasterData
-- import           Plexams.Import.Registrations
import           Plexams.PlanManip
import           Plexams.Types
import           Servant

semesterConfig' :: IO SemesterConfig
semesterConfig' = importSemesterConfig "plexams.yaml"

studentsFile' :: FilePath
studentsFile' = "input/students.yaml"

handicapsFile' :: FilePath
handicapsFile' = "input/handicaps.yaml"

-- planManipFile'' = "input/planmanip.yaml"

appliedPlan :: IO Plan
appliedPlan = do
  plan' <- liftIO intialPlan'
  semesterConfig'' <- liftIO semesterConfig'
  appliedPlan' <- applyPlanManips' (planManipFile semesterConfig'') plan'
  return appliedPlan'


intialPlan' :: IO Plan
intialPlan' = do
  semesterConfig'' <- liftIO semesterConfig'
  exams'' <- liftIO (importExams semesterConfig'')
  persons'' <- liftIO (importPersons semesterConfig'')
  students'' <- liftIO (importStudents (Just studentsFile'))
  handicaps'' <- liftIO (importHandicaps (Just handicapsFile'))
  return (Plexams.PlanManip.makePlan exams''  semesterConfig'' persons'' students'' handicaps'')

type API = "exams" :> Get '[JSON] [Exam]
      :<|> "examDays" :> Get '[JSON] [Day]
      :<|> "slots" :> Get '[JSON] Slots
      :<|> "slotsPerDay" :> Get '[JSON] [String]


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = exams'
    :<|> examDays'
    :<|> slots'
    :<|> slotsPerDay'

      where exams' :: Handler [Exam]
            exams' = do
              semesterConfig'' <- liftIO semesterConfig'
              exams'' <- liftIO (importExams semesterConfig'')
              return exams''

            examDays' :: Handler [Day]
            examDays' = do
              semesterConfig'' <- liftIO semesterConfig'
              return (examDays semesterConfig'')

            slots' :: Handler Slots
            slots' = do
              plan'' <- liftIO appliedPlan
              return (slots plan'')

            slotsPerDay' :: Handler [String]
            slotsPerDay' = do
              semesterConfig'' <- liftIO semesterConfig'
              return (slotsPerDay semesterConfig'')

-- eliminate :: Maybe a -> a
-- eliminate (Just a) = a
-- eliminate Nothing  = undefined
