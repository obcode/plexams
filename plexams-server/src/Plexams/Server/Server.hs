{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings   #-}


module Plexams.Server.Server
  ( startApp
  )
where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.List                      ( nub
                                                , partition
                                                , sortBy
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           GHC.Exts                       ( sortWith )

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors

import           Servant

import           Plexams.Import
import           Plexams.Invigilation
import           Plexams.PlanManip
import           Plexams.Query                  ( conflictingSlotsForAncode
                                                , queryRoomByID
                                                )
import           Plexams.Types
import           Plexams.UpdateFiles
import           Plexams.Validation

type API
   = "exams" :> Get '[ JSON] [Exam]
      :<|> "exam" :> ReqBody '[ JSON] Integer :> Post '[ JSON] (Maybe Exam)
      :<|> "examDays" :> Get '[ JSON] [String]
      :<|> "slots" :> Get '[ JSON] Slots
      :<|> "slot" :> ReqBody '[ JSON] (Int, Int) :> Post '[ JSON] [Exam]
      :<|> "slotsPerDay" :> Get '[ JSON] [String]
      :<|> "slotsForDay" :> ReqBody '[ JSON] Int :> Post '[ JSON] Slots
      :<|> "conflictingSlots" :> ReqBody '[ JSON] Integer :> Post '[ JSON]
                    ([( Int , Int)], [( Int , Int)])
      :<|> "addExam" :> ReqBody '[ JSON] AddExamToSlot :> Post '[ JSON] ()
      :<|> "unscheduledExams" :> Get '[ JSON] [Exam]
      :<|> "notPlannedByMeExams" :> Get '[ JSON] [Ancode]
      :<|> "validation" :> ReqBody '[ JSON] [ValidateWhat] :> Post '[ JSON] Validation
      :<|> "examsBySameLecturer" :> ReqBody '[ JSON] Ancode :> Post '[ JSON] [Exam]
      :<|> "goSlots" :> Get '[ JSON] [(Int, Int)]
      :<|> "lecturer" :> Get '[ JSON] [Person]
      :<|> "validateWhat" :> Get '[ JSON] [ValidateWhat]
      :<|> "reloadPlan" :> Get '[ JSON] (Bool, [Text])
      :<|> "plan" :> Get '[ JSON] Plan
      :<|> "semesterConfig" :> Get '[ JSON] SemesterConfig
      :<|> "invigilators" :> Get '[ JSON] (Invigilations, [Invigilator])
      :<|> "invigilatorsForDay" :> ReqBody '[ JSON] Int :> Post '[ JSON] ([Invigilator], [Invigilator])
      :<|> "addInvigilator" :> ReqBody '[ JSON] AddInvigilatorToRoomOrSlot :> Post '[ JSON] ()
      :<|> "removeInvigilator" :> ReqBody '[ JSON] RemoveInvigilatorFromRoomOrSlot :> Post '[ JSON] ()
      :<|> "examsWithNTA" :> Get '[ JSON] [Exam]
      :<|> "plannedRooms" :> Get '[ JSON] [PlannedRoomWithSlots]

newtype State = State
  { plan :: TVar Plan
  }

type StateHandler = ReaderT State Handler

stateHandlerToHandler :: State -> StateHandler a -> Handler a
stateHandlerToHandler state = stateHandlerToHandler'
 where
  stateHandlerToHandler' :: StateHandler a -> Handler a
  stateHandlerToHandler' h = runReaderT h state

startApp :: IO ()
startApp = do
  setPlexamsDirectory
  plan'    <- getPlan
  planTVar <- atomically $ newTVar plan'
  let state = State planTVar
  run 8080 (app state)

getPlan :: IO Plan
getPlan = do
  (maybePlan, errorMessages) <- importPlan
  mapM_ (putStrLn . unpack) errorMessages
  case maybePlan of
    Just plan' -> return plan'
    Nothing    -> return $ error "no plan"

app :: State -> Application
app = cors (const . Just $ corsPolicy) . serve api . server
 where
  corsPolicy = simpleCorsResourcePolicy
    { corsRequestHeaders = ["authorization", "content-type"]
    }

api :: Proxy API
api = Proxy

server :: State -> Server API
server state =
  hoistServer (Proxy :: Proxy API) (stateHandlerToHandler state)
    $    exams'
    :<|> exam'
    :<|> examDays'
    :<|> slots'
    :<|> slot'
    :<|> slotsPerDay'
    :<|> slotsForDay'
    :<|> conflictingSlots'
    :<|> addExam'
    :<|> unscheduledExams'
    :<|> notPlannedByMeExams'
    :<|> validation'
    :<|> examsBySameLecturer'
    :<|> goSlots'
    :<|> lecturer'
    :<|> validateWhat'
    :<|> reloadPlan'
    :<|> plan'
    :<|> semesterConfig'
    :<|> invigilators'
    :<|> invigilatorsForDay'
    :<|> addInvigilator
    :<|> removeInvigilator
    :<|> examsWithNTA'
    :<|> plannedRooms'
 where

  validateWhat' :: StateHandler [ValidateWhat]
  validateWhat' = return validateWhat

  plan' :: StateHandler Plan
  plan' = do
    State { plan = planT } <- ask
    liftIO $ readTVarIO planT

  semesterConfig' :: StateHandler SemesterConfig
  semesterConfig' = do
    State { plan = planT } <- ask
    liftIO $ semesterConfig <$> readTVarIO planT

  exams' :: StateHandler [Exam]
  exams' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ allExams plan''

  exam' :: Integer -> StateHandler (Maybe Exam)
  exam' examid = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ listToMaybe $ filter ((== examid) . anCode) $ allExams plan''

  examsWithNTA' :: StateHandler [Exam]
  examsWithNTA' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return
      $ sortWith (personShortName . lecturer)
      $ filter (not . null . handicapStudents)
      $ allExamsPlannedByMe plan''

  invigilators' :: StateHandler (Invigilations, [Invigilator])
  invigilators' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return (mkInvigilations plan'', M.elems $ invigilators plan'')

  invigilatorsForDay' :: Int -> StateHandler ([Invigilator], [Invigilator])
  invigilatorsForDay' dayIndex = do
    State { plan = planT } <- ask
    invigilators'' <- liftIO $ M.elems . invigilators <$> readTVarIO planT
    let wantAndPlannedInvigs = filter
          (\i ->
            dayIndex
              `elem` invigilatorWantDays i
              ||     dayIndex
              `elem` invigilatorInvigilationDays i
          )
          invigilators''
        canInvigs = filter
          (\i ->
            dayIndex
              `elem`    invigilatorCanDays i
              &&        dayIndex
              `notElem` invigilatorInvigilationDays i
          )
          invigilators''
    return (wantAndPlannedInvigs, canInvigs)

  examDays' :: StateHandler [String]
  examDays' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ examDaysAsStrings $ semesterConfig plan''

  slots' :: StateHandler Slots
  slots' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ slots plan''

  slot' :: (Int, Int) -> StateHandler [Exam]
  slot' s = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ maybe [] (M.elems . examsInSlot) $ M.lookup s $ slots plan''

  slotsPerDay' :: StateHandler [String]
  slotsPerDay' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ slotsPerDay $ semesterConfig plan''

  slotsForDay' :: Int -> StateHandler Slots
  slotsForDay' dayIndex = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ M.filterWithKey (\k _ -> fst k == dayIndex) $ slots plan''

  conflictingSlots' :: Ancode -> StateHandler ([(Int, Int)], [(Int, Int)])
  conflictingSlots' ancode = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ conflictingSlotsForAncode ancode plan''

  unscheduledExams' :: StateHandler [Exam]
  unscheduledExams' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return
      $ sortBy (\e1 e2 -> compare (registrations e2) (registrations e1))
      $ filter isUnscheduled
      $ allExams plan''

  notPlannedByMeExams' :: StateHandler [Ancode]
  notPlannedByMeExams' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ map head $ importedExams $ semesterConfig plan''

  examsBySameLecturer' :: Ancode -> StateHandler [Exam]
  examsBySameLecturer' anCode' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    let allExams'              = allExams plan''
        (thisExam, otherExams) = partition ((== anCode') . anCode) allExams'
        lecturer''             = personID $ lecturer $ head thisExam
        otherExams' = filter ((== lecturer'') . personID . lecturer) otherExams
    return otherExams'

  plannedRooms' :: StateHandler [PlannedRoomWithSlots]
  plannedRooms' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return
      $ map ((`queryRoomByID` plan'') . availableRoomName)
      $ availableRooms
      $ semesterConfig plan''

  validation' :: [ValidateWhat] -> StateHandler Validation
  validation' validateWhat'' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ uncurry Validation $ validate validateWhat'' plan''

  goSlots' :: StateHandler [(Int, Int)]
  goSlots' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ goSlots $ semesterConfig plan''

  lecturer' :: StateHandler [Person]
  lecturer' = do
    State { plan = planT } <- ask
    plan''                 <- liftIO $ readTVarIO planT
    return $ sortWith personShortName $ nub $ map lecturer $ allExams plan''

  addExam' :: AddExamToSlot -> StateHandler ()
  addExam' addExamToSlot' = do
    State { plan = planT } <- ask
    plan''''               <- liftIO $ atomically $ do
      plan'' <- readTVar planT
      let plan''' = applyAddExamToSlotListToPlan plan'' [addExamToSlot']
      writeTVar planT plan'''
      return plan'''
    let planManipFile' = planManipFile $ files $ semesterConfig plan''''
    case planManipFile' of
      Just planManipFile'' ->
        liftIO $ updatePlanManipFile planManipFile'' addExamToSlot'
      Nothing ->
        liftIO $ putStrLn $ "error while trying to add " ++ show addExamToSlot'

  addInvigilator :: AddInvigilatorToRoomOrSlot -> StateHandler ()
  addInvigilator addinvig@(AddInvigilatorToRoomOrSlot invigID invigSlot invigRoom)
    = do
      State { plan = planT } <- ask
      plan''''               <- liftIO $ atomically $ do
        plan'' <- readTVar planT
        let plan''' =
              addInvigilatorToExamOrSlot invigID invigSlot invigRoom plan''
        writeTVar planT plan'''
        return plan'''
      let invigilationsFile' =
            invigilationsFile $ files $ semesterConfig plan''''
      case invigilationsFile' of
        Just invigilationsFile'' ->
          liftIO $ updateInvigilationFile invigilationsFile'' addinvig
        Nothing ->
          liftIO $ putStrLn $ "error while trying to add " ++ show addinvig

  removeInvigilator :: RemoveInvigilatorFromRoomOrSlot -> StateHandler ()
  removeInvigilator (RemoveInvigilatorFromRoomOrSlot invigID invigSlot invigRoom)
    = do
      State { plan = planT } <- ask
      liftIO $ atomically $ do
        plan'' <- readTVar planT
        let plan''' =
              removeInvigilatorFromExamOrSlot invigID invigSlot invigRoom plan''
        writeTVar planT plan'''

  reloadPlan' :: StateHandler (Bool, [Text])
  reloadPlan' = do
    State { plan = planT }     <- ask
    (maybePlan, errorMessages) <- liftIO importPlan
    newPlanSet                 <- case maybePlan of
      Nothing      -> return False
      Just newPlan -> liftIO $ atomically $ do
        writeTVar planT newPlan
        return True
    return (newPlanSet, errorMessages)
