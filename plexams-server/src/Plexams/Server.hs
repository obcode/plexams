{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Plexams.Server
  ( startApp
  ) where

import           Control.Monad.Except
import           Data.Time.Calendar
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Plexams.Import.MasterData
import           Plexams.Types
import           Servant

semesterConfig' :: IO (Maybe SemesterConfig)
semesterConfig' = importSemesterConfigFromYAMLFile "plexams.yaml"

type API = "exams" :> Get '[JSON] [Exam]
      :<|> "examDays" :> Get '[JSON] [Day]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = exams'
    :<|> examDays'

      where exams' :: Handler [Exam]
            exams' = do
              config'' <- liftIO semesterConfig'
              exams'' <- liftIO (importExamsFromJSONFile (initialPlanFile (eliminate config'')))
              undefined <- liftIO $ print ( (eliminate config''))
              return (eliminate exams'')

            examDays' :: Handler [Day]
            examDays' = do
              config'' <- liftIO semesterConfig'
              undefined <- liftIO $ print ( (eliminate config''))
              return (examDays (eliminate config''))

eliminate :: Maybe a -> a
eliminate (Just a) = a
eliminate Nothing  = undefined
