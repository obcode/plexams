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
import           Plexams.Server.Check
import           Plexams.Server.Import
import           Plexams.Server.PlanManip
import           Plexams.Types
import           Servant

type API = "exams" :> Get '[JSON] [Exam]
      :<|> "examDays" :> Get '[JSON] [Day]
      :<|> "slots" :> Get '[JSON] Slots
      :<|> "slotsPerDay" :> Get '[JSON] [String]
      :<|> "addExam" :> ReqBody '[JSON] AddExamToSlot :> Post '[JSON] CheckError
      :<|> "unscheduledExams" :> Get '[JSON] (M.Map Ancode Exam)
      -- :<|> "validate" :> Get '[JSON] String

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
    -- :<|> validate'

      where
        exams' :: Handler [Exam]
        exams' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> (failingHandler $pack errorMsg)
            Right plan''' ->   return (allExams plan''')

        examDays' :: Handler [Day]
        examDays' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg  -> failingHandler $pack errorMsg
            Right config'' -> return (examDays config'')

        slots' :: Handler Slots
        slots' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> (failingHandler $pack errorMsg)
            Right plan''' ->   return (slots plan''')

        slotsPerDay' :: Handler [String]
        slotsPerDay' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg  -> (failingHandler $pack errorMsg)
            Right config'' -> return (slotsPerDay config'')

        addExam' exam = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg   -> (failingHandler $pack errorMsg)
            Right plan''' ->   do
              constraints' <- return $ constraints plan'''
              check <- return $ checkConstraints constraints' plan''' exam
              case check of
                Ok -> do
                  liftIO $ updateManipFile planManipFile' exam
                  return check
                _ -> return check

        unscheduledExams' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> (failingHandler $pack errorMsg)
            Right plan''' ->   return (unscheduledExams plan''')

failingHandler :: MonadError ServantErr m => ByteString -> m a
failingHandler s = throwError myerr
  where myerr :: ServantErr
        myerr = err503 { errBody = s }
