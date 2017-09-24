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
import           Data.Maybe
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
      :<|> "overlaps" :> ReqBody '[JSON] Ancode :> Post '[JSON] [Overlaps]

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
    :<|> overlaps'

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

        addExam' :: AddExamToSlot -> Handler CheckError
        addExam' exam = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg   -> (failingHandler $pack errorMsg)
            Right plan''' ->   do
              check <- return $ checkConstraints plan''' exam
              case check of
                Ok -> do
                  liftIO $ updateManipFile planManipFile' exam
                  return check
                _ -> return check

        unscheduledExams' :: Handler (M.Map Ancode Exam)
        unscheduledExams' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> (failingHandler $pack errorMsg)
            Right plan''' ->   return (unscheduledExams plan''')

        overlaps' :: Ancode -> Handler [Overlaps]
        overlaps' anCode' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> (failingHandler $pack errorMsg)
            Right plan''' -> do
              overlaps'' <- return $ overlaps $ constraints plan'''
              return $ filterOverlaps anCode' overlaps''

failingHandler :: MonadError ServantErr m => ByteString -> m a
failingHandler s = throwError myerr
  where myerr :: ServantErr
        myerr = err503 { errBody = s }

filterOverlaps :: Ancode -> [Overlaps] -> [Overlaps]
filterOverlaps anCode' overlaps' = Prelude.map overlap groups'
  where groups' = Prelude.filter
          (\overlap' -> (isJust (M.lookup anCode' (olOverlaps  overlap')))) overlaps'
        overlap group' = Overlaps
          { olGroup = olGroup group'
          , olOverlaps = olOverlaps' group'}
        olOverlaps' group' =
          M.fromList $ Prelude.filter
            (\overlap' -> (fst overlap') == anCode') (M.toList (olOverlaps group'))
