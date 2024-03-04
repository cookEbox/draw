{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Cairo as Cairo
import GI.Cairo.Render           as Ren hiding (x, y)
import GI.Cairo.Render.Connector as Con
import Data.Maybe
import GI.Gio
import Data.IORef
import Control.Applicative 
import Control.Monad
import System.Exit
import System.Environment

backGroundColor :: Render ()
backGroundColor = Ren.setSourceRGB 0 0 0 

penColor :: Render ()
penColor = Ren.setSourceRGB 255 255 255

handleDraw :: IORef (Maybe Surface) -> Gtk.DrawingArea 
           -> Cairo.Context -> IO Bool 
handleDraw surfaceRef _ cairoContext = do 
  readIORef surfaceRef >>= \case 
    Just surface -> do 
      Con.renderWithContext (Ren.setSourceSurface surface 0 0) cairoContext 
      Con.renderWithContext Ren.paint cairoContext
    Nothing -> return ()
  return True

updateSurface :: IORef (Maybe (Double, Double)) 
              -> Surface -> Double -> Double -> IO ()
updateSurface lastPosRef surface newX newY = do
  Ren.renderWith surface $ do
      Ren.setLineWidth 2 
      penColor

      lastPos <- liftIO $ readIORef lastPosRef 
      case lastPos of
        Just (lastX, lastY) -> do 
          Ren.moveTo lastX lastY 
          Ren.lineTo newX newY 
        Nothing -> Ren.moveTo newX newY 

      Ren.stroke
  writeIORef lastPosRef (Just (newX, newY))

setupDrawingArea :: IORef (Maybe Surface) -> IORef (Maybe (Double, Double)) 
                 -> IORef Bool -> Gtk.DrawingArea -> IO ()
setupDrawingArea surfaceRef lastPosRef isDrawingRef drawingArea = do
  -- Setup the draw event handler
  _ <- on drawingArea #draw $ handleDraw surfaceRef drawingArea

  -- Handle mouse button press
  _ <- on drawingArea #buttonPressEvent $ \_ -> do 
    writeIORef isDrawingRef True
    return False

  -- Handle mouse button release
  _ <- on drawingArea #buttonReleaseEvent $ \_ -> do 
    writeIORef isDrawingRef False
    writeIORef lastPosRef Nothing
    return False

  -- Handle mouse movement
  _ <- on drawingArea #motionNotifyEvent $ \event -> do
    isDrawing <- readIORef isDrawingRef
    when isDrawing $ do 
      x <- get event #x 
      y <- get event #y 
      surface <- fromJust <$> readIORef surfaceRef 
      updateSurface lastPosRef surface x y
      #queueDraw drawingArea -- Request a redraw
    return True

  -- Initialize the surface on realize
  _ <- on drawingArea #realize $ do
    nWidth <- fromIntegral <$> #getAllocatedWidth drawingArea
    nHeight <- fromIntegral <$> #getAllocatedHeight drawingArea
    when (nWidth > 0 && nHeight > 0) $ do
      surface <- Ren.createImageSurface Ren.FormatARGB32 nWidth nHeight
      Ren.renderWith surface $ backGroundColor >> Ren.paint -- Initialize with white background
      writeIORef surfaceRef (Just surface)
      
  return ()

initialiseSurface :: IORef (Maybe Surface) -> Gtk.DrawingArea -> IO ()
initialiseSurface surfaceRef drawingArea = do 
    _ <- readIORef surfaceRef >>= \case 
      Just _ -> return ()
      Nothing -> do 
        nWidth <- fromIntegral <$> #getAllocatedWidth drawingArea
        nHeight <- fromIntegral <$> #getAllocatedHeight drawingArea
        s <- Ren.createImageSurface Ren.FormatARGB32 nWidth nHeight
        Ren.renderWith s $ backGroundColor >> Ren.paint -- Initialize with white background
        writeIORef surfaceRef (Just s)
        return ()
    return ()

activate :: Gtk.Application -> ApplicationActivateCallback 
activate app = do 
  surfaceRef <- newIORef Nothing 
  lastPosRef <- newIORef Nothing
  isDrawingRef <- newIORef False

  drawingArea <- new Gtk.DrawingArea [ #heightRequest := 600
                                     , #widthRequest := 600
                                     ]

  #addEvents drawingArea [ Gdk.EventMaskButtonPressMask 
                         , Gdk.EventMaskPointerMotionMask 
                         , Gdk.EventMaskButtonReleaseMask
                         ]

  setupDrawingArea surfaceRef lastPosRef isDrawingRef drawingArea

  win <- new Gtk.ApplicationWindow [ #application := app 
                                   , #title := "Drawing Area"
                                   , On #destroy (Gtk.widgetDestroy ?self) 
                                   , #child := drawingArea
                                   ]


  #showAll win

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "org.gtk.example"
                             , On #activate (activate ?self)
                             ] 

  argv <- liftA2 (:) getProgName getArgs 
  stas <- #run app $ Just argv

  when (stas /= 0) $
    exitWith $ ExitFailure (fromIntegral stas)
