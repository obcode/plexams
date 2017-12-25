{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Plexams.Server.Server
  ( startApp
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM
import Data.List (nub, partition, sortBy)
import qualified Data.Map as M
import Data.Text (Text, unpack)
import GHC.Exts (sortWith)
import Network.Wai
import Network.Wai.Handler.Warp
import Plexams.Import
import Plexams.PlanManip
import Plexams.Server.PlanManip
import Plexams.Types
import Plexams.Validation
import Servant

type API
   = "exams" :> Get '[ JSON] [Exam] :<|> "examDays" :> Get '[ JSON] [String] :<|> "slots" :> Get '[ JSON] Slots :<|> "slot" :> ReqBody '[ JSON] ( Int
                                                                                                                                                , Int) :> Post '[ JSON] [Exam] :<|> "slotsPerDay" :> Get '[ JSON] [String] :<|> "slotsForDay" :> ReqBody '[ JSON] Int :> Post '[ JSON] Slots :<|> "addExam" :> ReqBody '[ JSON] AddExamToSlot :> Post '[ JSON] () :<|> "unscheduledExams" :> Get '[ JSON] [Exam] :<|> "notPlannedByMeExams" :> Get '[ JSON] [Ancode] :<|> "validation" :> ReqBody '[ JSON] [ValidateWhat] :> Post '[ JSON] Validation :<|> "examsBySameLecturer" :> ReqBody '[ JSON] Ancode :> Post '[ JSON] [Exam] :<|> "goSlots" :> Get '[ JSON] [( Int
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    , Int)] :<|> "lecturer" :> Get '[ JSON] [Person] :<|> "validateWhat" :> Get '[ JSON] [ValidateWhat] :<|> "reloadPlan" :> Get '[ JSON] ( Bool
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          , [Text]) :<|> "plan" :> Get '[ JSON] Plan :<|> "semesterConfig" :> Get '[ JSON] SemesterConfig :<|> "invigilators" :> Get '[ JSON] [Invigilator]
      -- :<|> "invigilatorsForDay" :> ReqBody '[JSON] Int :> Post '[JSON] ([Invigilator], [Invigilator])
      :<|> "examsWithNTA" :> Get '[ JSON] [Exam]

newtype State = State
  { plan :: TVar Plan
  }

type StateHandler = ReaderT State Handler

stateHandlerToHandler :: State -> StateHandler :~> Handler
stateHandlerToHandler state = NT stateHandlerToHandler'
  where
    stateHandlerToHandler' :: StateHandler a -> Handler a
    stateHandlerToHandler' h = runReaderT h state

startApp :: IO ()
startApp = do
  plan' <- getPlan
  planTVar <- atomically $ newTVar plan'
  let state = State planTVar
  run 8080 (app state)

getPlan :: IO Plan
getPlan = do
  (maybePlan, errorMessages) <- importPlan
  mapM_ (putStrLn . unpack) errorMessages
  case maybePlan of
    Just plan' -> return plan'
    Nothing -> return $ error "no plan"

app :: State -> Application
app = serve api . server

api :: Proxy API
api = Proxy

server :: State -> Server API
server state =
  enter (stateHandlerToHandler state) $
  exams' :<|> examDays' :<|> slots' :<|> slot' :<|> slotsPerDay' :<|>
  slotsForDay' :<|>
  addExam' :<|>
  unscheduledExams' :<|>
  notPlannedByMeExams' :<|>
  validation' :<|>
  examsBySameLecturer' :<|>
  goSlots' :<|>
  lecturer' :<|>
  validateWhat' :<|>
  reloadPlan' :<|>
  plan' :<|>
  semesterConfig' :<|>
  invigilators' :<|>
  examsWithNTA'
  where
    validateWhat' :: StateHandler [ValidateWhat]
    validateWhat' = return validateWhat
    plan' :: StateHandler Plan
    plan' = do
      State {plan = planT} <- ask
      liftIO $ atomically $ readTVar planT
    semesterConfig' :: StateHandler SemesterConfig
    semesterConfig' = do
      State {plan = planT} <- ask
      liftIO $ fmap semesterConfig $ atomically $ readTVar planT
    exams' :: StateHandler [Exam]
    exams' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ allExams plan''
    examsWithNTA' :: StateHandler [Exam]
    examsWithNTA' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $
        sortWith (personShortName . lecturer) $
        filter (not . null . handicapStudents) $ allExams plan''
    invigilators' :: StateHandler [Invigilator]
    invigilators' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ M.elems $ invigilators plan''
        -- invigilatorsForDay' :: Int -> StateHandler ([Invigilator],[Invigilator])
        -- invigilatorsForDay' dayIndex = do
        --   State { plan = planT } <- ask
        --   plan'' <- liftIO $ atomically $ readTVar planT
        --   return $ M.elems $ invigilators plan''
    examDays' :: StateHandler [String]
    examDays' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ examDaysAsStrings $ semesterConfig plan''
    slots' :: StateHandler Slots
    slots' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ slots plan''
    slot' :: (Int, Int) -> StateHandler [Exam]
    slot' s = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ maybe [] (M.elems . examsInSlot) $ M.lookup s $ slots plan''
    slotsPerDay' :: StateHandler [String]
    slotsPerDay' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ slotsPerDay $ semesterConfig plan''
    slotsForDay' :: Int -> StateHandler Slots
    slotsForDay' dayIndex = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ M.filterWithKey (\k _ -> fst k == dayIndex) $ slots plan''
    unscheduledExams' :: StateHandler [Exam]
    unscheduledExams' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $
        sortBy (\e1 e2 -> compare (registrations e2) (registrations e1)) $
        filter isUnscheduled $ allExams plan''
    notPlannedByMeExams' :: StateHandler [Ancode]
    notPlannedByMeExams' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ map head $ importedExams $ semesterConfig plan''
    examsBySameLecturer' :: Ancode -> StateHandler [Exam]
    examsBySameLecturer' anCode' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      let allExams' = allExams plan''
          (thisExam, otherExams) = partition ((== anCode') . anCode) allExams'
          lecturer'' = personID $ lecturer $ head thisExam
          otherExams' =
            filter ((== lecturer'') . personID . lecturer) otherExams
      return otherExams'
    validation' :: [ValidateWhat] -> StateHandler Validation
    validation' validateWhat'' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ uncurry Validation $ validate validateWhat'' plan''
    goSlots' :: StateHandler [(Int, Int)]
    goSlots' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ goSlots $ semesterConfig plan''
    lecturer' :: StateHandler [Person]
    lecturer' = do
      State {plan = planT} <- ask
      plan'' <- liftIO $ atomically $ readTVar planT
      return $ sortWith personShortName $ nub $ map lecturer $ allExams plan''
    addExam' :: AddExamToSlot -> StateHandler ()
    addExam' addExamToSlot' = do
      State {plan = planT} <- ask
      plan'''' <-
        liftIO $
        atomically $ do
          plan'' <- readTVar planT
          let plan''' = applyAddExamToSlotListToPlan plan'' [addExamToSlot']
          writeTVar planT plan'''
          return plan'''
      let planManipFile' = planManipFile $ files $ semesterConfig plan''''
      case planManipFile' of
        Just planManipFile'' -> do
          liftIO $ updateManipFile planManipFile'' addExamToSlot'
          return ()
        Nothing -> do
          liftIO $
            putStrLn $ "error while trying to add " ++ show addExamToSlot'
          return ()
    reloadPlan' :: StateHandler (Bool, [Text])
    reloadPlan' = do
      State {plan = planT} <- ask
      (maybePlan, errorMessages) <- liftIO importPlan
      newPlanSet <-
        case maybePlan of
          Nothing -> return False
          Just newPlan ->
            liftIO $
            atomically $ do
              writeTVar planT newPlan
              return True
      return (newPlanSet, errorMessages)
