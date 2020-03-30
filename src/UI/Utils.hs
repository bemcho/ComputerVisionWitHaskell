module UI.Utils
  ( initUIBuilder
  , getToolbarWindow
  , getDrawingAreaWindow
  , runApp
  ) where

import           Control.Monad.IO.Class
import           Graphics.UI.Gtk         hiding (Action, backspace)
import           Graphics.UI.Gtk.Builder

runApp::IO()
runApp = do
  initGUI -- (1)
  --todo embed .glade file in executable and use relative path
  builder <-
    initUIBuilder "/home/bemcho/Projects/HaskellWorld/CVWithHaskell/resource/ui/ComputerVisionWitHaskell_UI.glade"
  
  windowToolbar <- getToolbarWindow builder
  windowDrawingArea <- getDrawingAreaWindow builder
  
  widgetShowAll windowToolbar
  widgetShowAll windowDrawingArea
  mainGUI
  
initUIBuilder :: String -> IO Builder
initUIBuilder path = do
  builder <- builderNew
  builderAddFromFile builder path
  return builder

getToolbarWindow :: Builder -> IO Window
getToolbarWindow b = do 
  window <- builderGetObject b castToWindow "main_window_toolbar"
  window `on` deleteEvent $ -- handler to run on window destruction
          liftIO mainQuit >>
          return False
  return window

getDrawingAreaWindow :: Builder -> IO Window
getDrawingAreaWindow b = builderGetObject b castToWindow "main_window_draw"
