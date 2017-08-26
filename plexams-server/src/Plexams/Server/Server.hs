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
      :<|> "addExam" :> ReqBody '[JSON] AddExamToSlot :> Post '[JSON] String
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
            Left errorMsg   -> (failingHandler $pack errorMsg)
            Right plan''' ->   return (allExams plan''')

        examDays' :: Handler [Day]
        examDays' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg    -> failingHandler $pack errorMsg
            Right config'' -> return (examDays config'')

        slots' :: Handler Slots
        slots' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg   -> (failingHandler $pack errorMsg)
            Right plan''' ->   return (slots plan''')

        slotsPerDay' :: Handler [String]
        slotsPerDay' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg    -> (failingHandler $pack errorMsg)
            Right config'' -> return (slotsPerDay config'')

        addExam' exam = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg   -> (failingHandler $pack errorMsg)
            Right plan''' ->   do
              constraints' <- return $ constraints plan'''
              check <- return $ checkFixedSlot (fixedSlot constraints') exam
              case check of
                Left errorMsg -> return errorMsg
                Right response -> do
                  liftIO $ updateManipFile  planManipFile' exam
                  return response

        unscheduledExams' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg   -> (failingHandler $pack errorMsg)
            Right plan''' ->   return (unscheduledExams plan''')

        -- validate' = do
        --   plan'' <- liftIO appliedPlan
        --   case plan'' of
        --     Left errorMsg   -> (failingHandler $pack errorMsg)
        --     Right plan''' ->   return (show $ constraints plan''')

failingHandler :: MonadError ServantErr m => ByteString -> m a
failingHandler s = throwError myerr
  where myerr :: ServantErr
        myerr = err503 { errBody = s }

checkFixedSlot :: [(Integer, (Int, Int))] -> AddExamToSlot -> Either String String
checkFixedSlot fixedSlots exam =
  if ([] == entry')
    then Right "true"
    else if (entry == exam)
      then Right "true"
      else Left $ "Exam is bound to the fixed Slot: (Day: " ++ (show $ planManipDay entry)
                ++ " Slot: " ++ (show $ planManipSlot entry) ++ ")."
  where
    entry = AddExamToSlot {planManipAnCode = fst $ Prelude.head entry'
                          , planManipDay = fst $ snd $ Prelude.head entry'
                          , planManipSlot = snd $ snd $ Prelude.head entry' }
    entry' = Prelude.filter (\(x,_) -> x == planManipAnCode exam) fixedSlots
