module Plexams.GUI where

import                Plexams.Types
import qualified      Graphics.UI.Threepenny       as UI 
import                Graphics.UI.Threepenny.Core

mainGUI :: IO ()
mainGUI = do
   startGUI defaultConfig
       { jsPort       = Just 8023
       , jsStatic     = Just "../wwwroot"
       } setup

setup :: Window -> UI ()
setup window = do
  _ <- return window # set UI.title "Hello World!"
  button  <- UI.button # set UI.text "Click me!"
  _ <- getBody window #+ [element button]
  on UI.click button $ const $ do
      element button # set UI.text "I have been clicked!"