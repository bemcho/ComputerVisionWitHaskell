module UI.Utils
  ( initUIBuilder
  , getToolbarWindow
  , getDrawingAreaWindow
  , runApp
  ) where

import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text               as T
import           Graphics.UI.Gtk         hiding (Action, backspace)
import           Graphics.UI.Gtk.Builder

runApp :: IO ()
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
  window <- getWindow b "main_window_toolbar"
  window `on` deleteEvent $ -- handler to run on window destruction
    liftIO mainQuit >>
    return False
  openFileBtn <- getButton b "open_image_btn"
  openFileBtn `on` buttonActivated $ -- handler to run open image
   do
    maybePath <- runOpenImageDialog
    putStrLn (fromMaybe "" maybePath)
    return ()
  return window

getDrawingAreaWindow :: Builder -> IO Window
getDrawingAreaWindow b = getWindow b "main_window_draw"

getEditResizeWindow :: Builder -> IO Window
getEditResizeWindow b = getWindow b "edit_resize_window"

getEditCropWindow :: Builder -> IO Window
getEditCropWindow b = getWindow b "edit_crop_window"

getEditScaleWindow :: Builder -> IO Window
getEditScaleWindow b = getWindow b "edit_scale_window"

getWindow :: Builder -> String -> IO Window
getWindow b id = builderGetObject b castToWindow id

getButton :: Builder -> String -> IO Button
getButton b id = builderGetObject b castToButton id

getSliderAdjustment :: Builder -> String -> IO Adjustment
getSliderAdjustment b id = builderGetObject b castToAdjustment id

runOpenImageDialog :: IO (Maybe String)
runOpenImageDialog = do
  hsfilt <- fileFilterNew
  fileFilterAddPattern hsfilt "*.png"
  fileFilterAddPattern hsfilt "*.jpg"
  fileFilterAddPattern hsfilt "*.tiff"
  fileFilterAddPattern hsfilt "*.gif"
  fileFilterSetName hsfilt "Image Files"   
  
  fchdal <-
    fileChooserDialogNew
      (Just "Open image Dialog")
      Nothing
      FileChooserActionOpen
      [("Cancel", ResponseCancel), ("Open", ResponseAccept)]
  fileChooserAddFilter fchdal hsfilt    
  widgetShow fchdal
  response <- dialogRun fchdal
  case response of
    otherwise -> do
      widgetDestroy fchdal
      return Nothing
    ResponseAccept -> do
      nwf <- fileChooserGetFilename fchdal
      case nwf of
        Just path -> do
          widgetDestroy fchdal
          return (Just path)
