{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Plexams.Server.Server
  ( startApp
  ) where

import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8
import qualified Data.Map                   as M
import           Data.Time.Calendar
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Plexams.Server.Import
import           Plexams.Server.PlanManip
import           Plexams.Types
import           Servant

type API = "exams" :> Get '[JSON] [Exam]
      :<|> "examDays" :> Get '[JSON] [Day]
      :<|> "slots" :> Get '[JSON] Slots
      :<|> "slotsPerDay" :> Get '[JSON] [String]
      :<|> "addExam" :> ReqBody '[JSON] AddExam :> Post '[JSON] String
      :<|> "unscheduledExams" :> Get '[JSON] (M.Map Ancode Exam)

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
    :<|> addExam'
    :<|> unscheduledExams'

      where
        exams' :: Handler [Exam]
        exams' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left error' -> failingHandler $pack error'
            Right config'' -> do
              examsE <- liftIO (importExams config'')
              case examsE of
                Left error'   -> failingHandler $pack error'
                Right exams'' -> do
                  return exams''

        examDays' :: Handler [Day]
        examDays' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left error'    -> failingHandler $pack error'
            Right config'' -> return (examDays config'')

        slots' :: Handler Slots
        slots' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left error'   -> (failingHandler $pack error')
            Right plan''' ->   return (slots plan''')

        slotsPerDay' :: Handler [String]
        slotsPerDay' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left error'    -> (failingHandler $pack error')
            Right config'' -> return (slotsPerDay config'')

        addExam' exam = do
          emtpy <- liftIO $ appendManipFile  planManipFile' exam
          return  $ "- [" ++ show (anCode1 exam) ++ ", " ++ show (day exam) ++ ", " ++ show (slot1 exam) ++ "]"

        unscheduledExams' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left error'   -> (failingHandler $pack error')
            Right plan''' ->   return (unscheduledExams plan''')

failingHandler :: MonadError ServantErr m => ByteString -> m a
failingHandler s = throwError myerr
  where myerr :: ServantErr
        myerr = err503 { errBody = s }
