{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative       (Applicative (liftA2))
import           Control.Monad             (when)
import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           Data.Maybe                (fromJust)
import           Data.Text                 (pack)
import qualified GI.Cairo                  as Cairo
import qualified GI.Cairo.Render           as Ren
import qualified GI.Cairo.Render.Connector as Con
import qualified GI.Gdk                    as Gdk
import           GI.Gio
import qualified GI.Gtk                    as Gtk
import           System.Environment
import           System.Exit

{- Basic settings -}

backGroundColor :: Ren.Render ()
backGroundColor = Ren.setSourceRGB 0 0 0

penColor :: Ren.Render ()
penColor = Ren.setSourceRGB 255 255 255

pageWidth, pageHeight :: Integral a => a 
pageWidth = 2200
pageHeight = 1300

{- Handling Draw Trigger -}

handleDraw :: IORef (Maybe Ren.Surface) -> Gtk.DrawingArea
           -> Cairo.Context -> IO Bool
handleDraw surfaceRef _ cairoContext = do
  readIORef surfaceRef >>= \case
    Just surface -> do
      Con.renderWithContext (Ren.setSourceSurface surface 0 0) cairoContext
      Con.renderWithContext Ren.paint cairoContext
    Nothing -> return ()
  return True

{- Handling Drawing from mouse being clicked and dragged -}

updateSurface :: IORef (Maybe (Double, Double))
              -> Ren.Surface -> Double -> Double -> IO ()
updateSurface lastPosRef surface newX newY = do
  Ren.renderWith surface $ do
      Ren.setLineWidth 2
      penColor
      lastPos <- Ren.liftIO $ readIORef lastPosRef
      case lastPos of
        Just (lastX, lastY) -> do
          Ren.moveTo lastX lastY
          Ren.lineTo newX newY
        Nothing -> Ren.moveTo newX newY
      Ren.stroke
  writeIORef lastPosRef (Just (newX, newY))

{- Handle all the on drawingArea commands -}

setupDrawingArea :: IORef (Maybe Ren.Surface) -> IORef (Maybe (Double, Double))
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
    -- nWidth <- fromIntegral <$> #getAllocatedWidth drawingArea
    -- nHeight <- fromIntegral <$> #getAllocatedHeight drawingArea
    -- when (nWidth > 0 && nHeight > 0) $ do
      surface <- Ren.createImageSurface Ren.FormatARGB32 pageWidth pageHeight
      Ren.renderWith surface $ backGroundColor >> Ren.paint -- Initialize with white background
      writeIORef surfaceRef (Just surface)
  return ()

{- Start a surface on the activation of the app -}

initialiseSurface :: IORef (Maybe Ren.Surface) -> Gtk.DrawingArea -> IO ()
initialiseSurface surfaceRef drawingArea = do
    _ <- readIORef surfaceRef >>= \case
      Just _ -> return ()
      Nothing -> do
        -- nWidth <- fromIntegral <$> #getAllocatedWidth drawingArea
        -- nHeight <- fromIntegral <$> #getAllocatedHeight drawingArea
        s <- Ren.createImageSurface Ren.FormatARGB32 pageWidth pageHeight
        Ren.renderWith s $ backGroundColor >> Ren.paint -- Initialize with white background
        writeIORef surfaceRef (Just s)
        return ()
    return ()

addPage :: Gtk.Notebook -> Int -> IO ()
addPage notebook num = do
      surfaceRef <- newIORef Nothing
      lastPosRef <- newIORef Nothing
      isDrawingRef <- newIORef False
      pageLabel <- new Gtk.Label [ #label := pack $ "Page " ++ show num ]
      drawingArea <- new Gtk.DrawingArea [ #widthRequest := pageWidth
                                         , #heightRequest := pageHeight
                                         ]
      _ <- Gtk.notebookAppendPage notebook drawingArea (Just pageLabel)
      #addEvents drawingArea [ Gdk.EventMaskButtonPressMask
                             , Gdk.EventMaskPointerMotionMask
                             , Gdk.EventMaskButtonReleaseMask
                             ]
      setupDrawingArea surfaceRef lastPosRef isDrawingRef drawingArea
      #showAll notebook

{- Initialise and setup drawingArea and Window etc -}

activate :: Gtk.Application -> ApplicationActivateCallback
activate app = do
  notebook <- new Gtk.Notebook []
  mapM_ (addPage notebook) [1..3]
  scrolledWin <- new Gtk.ScrolledWindow [ #hscrollbarPolicy := Gtk.PolicyTypeAlways
                                        , #vscrollbarPolicy := Gtk.PolicyTypeAlways
                                        , #child := notebook
                                        ]
  win <- new Gtk.ApplicationWindow [ #application := app
                                   , #title := "Drawing Area"
                                   , On #destroy (Gtk.widgetDestroy ?self)
                                   , #child := scrolledWin
                                   , #defaultWidth := 1200 
                                   , #defaultHeight := 1000
                                   ]
  #showAll win

{- Starts application and handles closing error messages -}

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "org.gtk.example"
                             , On #activate (activate ?self)
                             ]
  argv <- liftA2 (:) getProgName getArgs
  stas <- #run app $ Just argv
  when (stas /= 0) $
    exitWith $ ExitFailure (fromIntegral stas)
