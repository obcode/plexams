{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Plexams.Server.Server
  ( startApp
  ) where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List                  (nub, partition, sortBy)
import qualified Data.Map                   as M
import           Data.Maybe
import           GHC.Exts                   (sortWith)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Plexams.PlanManip
import           Plexams.Server.Check
import           Plexams.Server.Import
import           Plexams.Server.PlanManip
import           Plexams.Types hiding (importExams)
import qualified Plexams.Types
import           Plexams.Validation
import           Servant


type API = "exams" :> Get '[JSON] [Exam]
      :<|> "studentregs" :> Get '[JSON] [StudentWithRegs]
      :<|> "examDays" :> Get '[JSON] [String]
      :<|> "slots" :> Get '[JSON] Slots
      :<|> "slot" :> ReqBody '[JSON] (String, String) :> Post '[JSON] [Exam]
      :<|> "slotsPerDay" :> Get '[JSON] [String]
      :<|> "slotsForDay" :> ReqBody '[JSON] Int :> Post '[JSON] Slots
      :<|> "addExam" :> ReqBody '[JSON] AddExamToSlot :> Post '[JSON] CheckError
      :<|> "unscheduledExams" :> Get '[JSON] [Exam]
      :<|> "notPlannedByMeExams" :> Get '[JSON] [Ancode]
      :<|> "overlaps" :> ReqBody '[JSON] Ancode :> Post '[JSON] [Overlaps]
      :<|> "validation" :> ReqBody '[JSON] [ValidateWhat] :> Post '[JSON] Validation
      :<|> "examsBySameLecturer" :> ReqBody '[JSON] Ancode :> Post '[JSON] [Exam]
      :<|> "goSlots" :> Get '[JSON] [(Int, Int)]
      :<|> "lecturer" :> Get '[JSON] [Person]
      :<|> "validateWhat" :> Get '[JSON] [ValidateWhat]

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
    :<|> slot'
    :<|> slotsPerDay'
    :<|> slotsForDay'
    :<|> addExam'
    :<|> unscheduledExams'
    :<|> notPlannedByMeExams'
    :<|> overlaps'
    :<|> validation'
    :<|> examsBySameLecturer'
    :<|> goSlots'
    :<|> lecturer'
    :<|> validateWhat'

      where

        getPlan :: IO Plan
        getPlan = do
          plan'' <- appliedPlan
          studentregs' <- importStudentRegistrations
          -- TODO: importInvigilators
          case plan'' of
            Left errorMsg -> error errorMsg
            Right plan''' -> return
              $ addStudentRegistrationsToPlan
                (fromMaybe M.empty studentregs')
                plan'''

        getSemesterConfig :: IO SemesterConfig
        getSemesterConfig = do
          semesterConfig'' <- semesterConfig'
          case semesterConfig'' of
            Left errorMsg  -> error errorMsg
            Right config'' -> return config''

        validateWhat' :: Handler [ValidateWhat]
        validateWhat' = return validateWhat

        exams' :: Handler [Exam]
        exams' = fmap allExams (liftIO getPlan)

        studentregs :: Handler [StudentWithRegs]
        studentregs = do
          studentregs' <- liftIO importStudentRegistrations
          return $ maybe [] M.elems studentregs'

        examDays' :: Handler [String]
        examDays' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg  -> failingHandler $ BSL.pack errorMsg
            Right config'' -> return $ examDaysAsStrings config''

        slots' :: Handler Slots
        slots' = fmap slots (liftIO getPlan)

        slot' :: (String, String) -> Handler [Exam]
        slot' (ds, ss) = do
          let s = (read ds, read ss) -- TODO: Catch errors
          plan'' <- liftIO getPlan
          return $ maybe [] (M.elems . examsInSlot)
                                    $ M.lookup s
                                    $ slots plan''

        slotsPerDay' :: Handler [String]
        slotsPerDay' = do
          semesterConfig'' <- liftIO semesterConfig'
          case semesterConfig'' of
            Left errorMsg  -> failingHandler $ BSL.pack errorMsg
            Right config'' -> return $ slotsPerDay config''

        slotsForDay' :: Int -> Handler Slots
        slotsForDay' dayIndex =
          fmap (M.filterWithKey (\k _ -> fst k == dayIndex) . slots)
               (liftIO getPlan)

        addExam' :: AddExamToSlot -> Handler CheckError
        addExam' exam = do
          plan'' <- liftIO appliedPlan
          case plan'' of
            Left errorMsg   -> failingHandler $ BSL.pack errorMsg
            Right plan''' ->
              let check = checkConstraints plan''' exam
              in case check of
                Ok -> do
                  liftIO $ updateManipFile planManipFile' exam
                  return check
                _ -> return check

        unscheduledExams' :: Handler [Exam]
        unscheduledExams' =
          fmap (sortBy (\e1 e2 -> compare (registrations e2)
                                          (registrations e1))
                . filter isUnscheduled
                . allExams)
               (liftIO getPlan)

        notPlannedByMeExams' :: Handler [Ancode]
        notPlannedByMeExams' =
          fmap (map head . Plexams.Types.importExams)
               (liftIO getSemesterConfig)

        overlaps' :: Ancode -> Handler [Overlaps]
        overlaps' anCode' =
          fmap (\plan'' -> filterOverlaps anCode'
                           $ (studentsOverlaps plan'' :)
                           $ overlaps
                           $ constraints plan''
               ) (liftIO getPlan)

        examsBySameLecturer' :: Ancode -> Handler [Exam]
        examsBySameLecturer' anCode' = do
          plan'' <- liftIO getPlan
          let allExams' = allExams plan''
              (thisExam, otherExams) =
                                partition ((==anCode') . anCode) allExams'
              lecturer'' = personID $ lecturer $ head thisExam
              otherExams' = filter
                            ((==lecturer'') . personID . lecturer) otherExams
          return otherExams'

        validation' :: [ValidateWhat] -> Handler Validation
        validation' validateWhat'' =
          fmap (uncurry Validation . validate validateWhat'')
               (liftIO getPlan)

        goSlots' :: Handler [(Int, Int)]
        goSlots' = fmap goSlots (liftIO getSemesterConfig)

        lecturer' :: Handler [Person]
        lecturer' = fmap (sortWith personShortName
                          . nub
                          . map lecturer
                          . allExams) (liftIO getPlan)


failingHandler :: MonadError ServantErr m => BSL.ByteString -> m a
failingHandler s = throwError myerr
  where myerr :: ServantErr
        myerr = err503 { errBody = s }

filterOverlaps :: Ancode -> [Overlaps] -> [Overlaps]
filterOverlaps anCode' overlaps' = map overlap groups'
  where groups' = filter
          (isJust . M.lookup anCode' . olOverlaps) overlaps'
        overlap group' = Overlaps
          { olGroup = olGroup group'
          , olOverlaps = olOverlaps' group'}
        olOverlaps' group' =
          M.fromList $ filter
            (\overlap' -> fst overlap' == anCode') (M.toList (olOverlaps group'))
