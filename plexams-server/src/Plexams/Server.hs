{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Plexams.Server
  ( startApp
  ) where

import           Control.Monad.Except
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Plexams.Import.MasterData
import           Plexams.Types
import           Servant

type API = "exams" :> Get '[JSON] [Exam]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = do
  exams'' <- liftIO (importExamsFromJSONFile "input/initialplan.json")
  return (eliminate exams'')
    --
eliminate :: Maybe a -> a
eliminate (Just a) = a
eliminate Nothing  = undefined
