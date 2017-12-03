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
import           Plexams.Export.Common
import           Plexams.PlanManip
import           Plexams.Server.Check
import           Plexams.Server.Import
import           Plexams.Server.PlanManip
import           Plexams.Types
import           Plexams.Validation
import           Servant

type API = "exams" :> Get '[JSON] [Exam]
      :<|> "studentregs" :> Get '[JSON] [StudentWithRegs]
      :<|> "examDays" :> Get '[JSON] [Day]
      :<|> "slots" :> Get '[JSON] Slots
      :<|> "slotsPerDay" :> Get '[JSON] [String]
      :<|> "addExam" :> ReqBody '[JSON] AddExamToSlot :> Post '[JSON] CheckError
      :<|> "unscheduledExams" :> Get '[JSON] [Exam]
      :<|> "overlaps" :> ReqBody '[JSON] Ancode :> Post '[JSON] [Overlaps]
      :<|> "validation" :> Get '[JSON] Validation

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = exams'
    :<|> studentregs
    :<|> examDays'
    :<|> slots'
    :<|> slotsPerDay'
    :<|> addExam'
    :<|> unscheduledExams'
    :<|> overlaps'
    :<|> validation'

      where
        exams' :: Handler [Exam]
        exams' = do
          plan'' <- liftIO appliedPlan
          regs' <- liftIO importRegistrations
          studentregs' <- liftIO importStudentRegistrations
          case plan'' of
            Left errorMsg -> failingHandler $ pack errorMsg
            Right plan''' -> return
                $ addStudentRegistrationsToExams
                    (fromMaybe M.empty studentregs')
                $ addRegistrationsListToExams (allExams plan''')
                $ fromMaybe [] regs'

        studentregs :: Handler [StudentWithRegs]
        studentregs = do
          studentregs' <- liftIO importStudentRegistrations
          return $ maybe [] M.elems studentregs'

        examDays' :: Handler [Day]
        examDays' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg  -> failingHandler $ pack errorMsg
            Right config'' -> return $ examDays config''

        slots' :: Handler Slots
        slots' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> failingHandler $ pack errorMsg
            Right plan''' -> return $ slots plan'''

        validation' :: Handler Validation
        validation' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> failingHandler $ pack errorMsg
            Right plan''' -> return $ uncurry Validation
                                    $ validate [ValidateSchedule] plan'''

        slotsPerDay' :: Handler [String]
        slotsPerDay' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg  -> failingHandler $ pack errorMsg
            Right config'' -> return $ slotsPerDay config''

        addExam' :: AddExamToSlot -> Handler CheckError
        addExam' exam = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg   -> failingHandler $ pack errorMsg
            Right plan''' ->
              let check = checkConstraints plan''' exam
              in case check of
                Ok -> do
                  liftIO $ updateManipFile planManipFile' exam
                  return check
                _ -> return check

        unscheduledExams' :: Handler [Exam]
        unscheduledExams' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> failingHandler $pack errorMsg
            Right plan''' -> return $ unscheduledExamsSortedByRegistrations plan'''

        overlaps' :: Ancode -> Handler [Overlaps]
        overlaps' anCode' = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg -> failingHandler $ pack errorMsg
            Right plan''' ->
              let overlaps'' = (studentsOverlaps plan''' :)
                                   $ overlaps $ constraints plan'''
              in return $ filterOverlaps anCode' overlaps''

failingHandler :: MonadError ServantErr m => ByteString -> m a
failingHandler s = throwError myerr
  where myerr :: ServantErr
        myerr = err503 { errBody = s }

filterOverlaps :: Ancode -> [Overlaps] -> [Overlaps]
filterOverlaps anCode' overlaps' = Prelude.map overlap groups'
  where groups' = Prelude.filter
          (isJust . M.lookup anCode' . olOverlaps) overlaps'
        overlap group' = Overlaps
          { olGroup = olGroup group'
          , olOverlaps = olOverlaps' group'}
        olOverlaps' group' =
          M.fromList $ Prelude.filter
            (\overlap' -> fst overlap' == anCode') (M.toList (olOverlaps group'))
