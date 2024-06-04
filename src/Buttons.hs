{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
module Buttons where

import           BasicSettings
import           Control.Monad             (when)
import           Data.IORef                (IORef, readIORef, writeIORef)
import           Data.Maybe                (fromJust)
import qualified GI.Cairo                  as Cairo
import qualified GI.Cairo.Render           as Ren
import qualified GI.Cairo.Render.Connector as Con
import qualified GI.Gdk                    as Gdk
import           GI.Gio
import qualified GI.Gtk                    as Gtk

{- Handling Draw Trigger -}

handleDraw :: IORef (Maybe Ren.Surface)
           -> Gtk.DrawingArea
           -> Cairo.Context
           -> IO Bool
handleDraw surfaceRef _ cairoContext = do
  readIORef surfaceRef >>= \case
    Just surface -> do
      Con.renderWithContext (Ren.setSourceSurface surface 0 0) cairoContext
      Con.renderWithContext Ren.paint cairoContext
    Nothing -> return ()
  return True

{- Handling Drawing from mouse being clicked and dragged -}

updateSurface :: IORef State
              -> Ren.Surface
              -> Double
              -> Double
              -> IO ()
updateSurface stateRef surface newX newY = do
  state <- readIORef stateRef
  let color = getPenColor state
      lastPos = getLastPos state
      penSize = (if color == Default then 20 else 2)
  Ren.renderWith surface $ do
      Ren.setLineWidth penSize
      penColor color
      case lastPos of
        Just (lastX, lastY) -> do
          Ren.moveTo lastX lastY
          Ren.lineTo newX newY
        Nothing -> Ren.moveTo newX newY
      Ren.stroke
  let newState = state { getLastPos = Just (newX, newY)}
  writeIORef stateRef newState

{- Handle all the on drawingArea commands -}

buttonPress :: Gdk.EventButton -> IORef State -> IO Bool
buttonPress _ stateRef = do
  state <- readIORef stateRef
  let newState = state { getIsDrawing = True }
  writeIORef stateRef newState
  return False

buttonRelease :: Gdk.EventButton
              -> IORef State
              -> IO Bool
buttonRelease _ stateRef = do
  state <- readIORef stateRef
  let newState = state { getIsDrawing = False, getLastPos = Nothing }
  writeIORef stateRef newState
  return False

motionNotify :: Gdk.EventMotion
             -> IORef (Maybe Ren.Surface)
             -> IORef State
             -> Gtk.DrawingArea
             -> IO Bool
motionNotify event surfaceRef stateRef drawingArea = do
  state <- readIORef stateRef
  when (getIsDrawing state) $ do
    x <- get event #x
    y <- get event #y
    surface <- fromJust <$> readIORef surfaceRef
    updateSurface stateRef surface x y
    #queueDraw drawingArea -- Request a redraw
  return True

realize :: IORef (Maybe Ren.Surface) -> IO ()
realize surfaceRef = do
  surface <- Ren.createImageSurface Ren.FormatARGB32 pageWidth pageHeight
  Ren.renderWith surface $ backGroundColor >> Ren.paint
  writeIORef surfaceRef (Just surface)
  return ()

rightClickNotify :: Gdk.EventButton -> Gtk.Menu -> IO Bool
rightClickNotify event menu = do
  button <- get event #button
  case button of
    3 -> do -- Right-click is button 3
         Gtk.menuPopupAtPointer menu Nothing
         return True
    _ -> return False


