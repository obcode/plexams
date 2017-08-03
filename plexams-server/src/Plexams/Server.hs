{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Plexams.Server
  ( startApp
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data Exam = Exam
  { name          :: Text
  , lecturer      :: Text
  , datetime      :: Text
  , registrations :: Integer
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Exam)

type API = "exams" :> Get '[JSON] [Exam]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return plexams

plexams :: [Exam]
plexams =
  [ Exam "Compiler" "Braun, O." "28.02.2018, 17:30" 25
  , Exam "Funktionale Programmierung" "Braun, O." "12.02.2018, 08:30" 18
  , Exam "Lineare Algebra" "Mustermann, M." "25.02.2018, 12:30" 25
  ]
