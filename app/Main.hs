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

data Color = Default | White | Blue | Red | Green | Black deriving (Show, Eq)

backGroundColor :: Ren.Render ()
backGroundColor = Ren.setSourceRGB 0 0 0

penColor :: Color -> Ren.Render ()
penColor Default = Ren.setSourceRGB 255 255 255
penColor White = Ren.setSourceRGB 255 255 255
penColor Blue = Ren.setSourceRGB 0 0 255
penColor Red = Ren.setSourceRGB 255 0 0 
penColor Green = Ren.setSourceRGB 0 255 0 
penColor Black = Ren.setSourceRGB 0 0 0 

pageWidth, pageHeight :: Integral a => a 
pageWidth = 2200
pageHeight = 1300

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

updateSurface :: IORef Color 
              -> IORef (Maybe (Double, Double))
              -> Ren.Surface 
              -> Double 
              -> Double 
              -> IO ()
updateSurface penColorRef lastPosRef surface newX newY = do
  color <- readIORef penColorRef
  let penSize = (if color == Black then 10 else 2)
  Ren.renderWith surface $ do
      Ren.setLineWidth penSize
      penColor color
      lastPos <- Ren.liftIO $ readIORef lastPosRef
      case lastPos of
        Just (lastX, lastY) -> do
          Ren.moveTo lastX lastY
          Ren.lineTo newX newY
        Nothing -> Ren.moveTo newX newY
      Ren.stroke
  writeIORef lastPosRef (Just (newX, newY))

{- Handle all the on drawingArea commands -}

buttonPress :: Gdk.EventButton -> IORef Bool -> IO Bool
buttonPress _ isDrawingRef = do 
  writeIORef isDrawingRef True
  return False

buttonRelease :: Gdk.EventButton 
              -> IORef Bool 
              -> IORef (Maybe (Double, Double)) 
              -> IO Bool
buttonRelease _ isDrawingRef lastPosRef = do
  writeIORef isDrawingRef False
  writeIORef lastPosRef Nothing
  return False

motionNotify :: Gdk.EventMotion 
             -> IORef (Maybe Ren.Surface) 
             -> IORef (Maybe (Double, Double))
             -> IORef Bool 
             -> IORef Color
             -> Gtk.DrawingArea 
             -> IO Bool
motionNotify event surfaceRef lastPosRef isDrawingRef penColorRef drawingArea = do
    isDrawing <- readIORef isDrawingRef
    when isDrawing $ do
      x <- get event #x
      y <- get event #y
      surface <- fromJust <$> readIORef surfaceRef
      updateSurface penColorRef lastPosRef surface x y
      #queueDraw drawingArea -- Request a redraw
    return True

realize :: IORef (Maybe Ren.Surface) -> IO ()
realize surfaceRef = do 
  surface <- Ren.createImageSurface Ren.FormatARGB32 pageWidth pageHeight
  Ren.renderWith surface $ backGroundColor >> Ren.paint 
  writeIORef surfaceRef (Just surface)
  return ()

rightClickMenu :: IORef Bool -> IORef Color -> IO Gtk.Menu
rightClickMenu isDrawingRef penColorRef = do 
  menu <- new Gtk.Menu [ On #hide $ writeIORef isDrawingRef False] 
  white <- new Gtk.MenuItem [ #label := "White" 
                            , On #activate $ writeIORef penColorRef White 
                            ]
  red <- new Gtk.MenuItem   [ #label := "Red" 
                            , On #activate $ writeIORef penColorRef Red 
                            ]
  blue <- new Gtk.MenuItem [ #label := "Blue" 
                            , On #activate $ writeIORef penColorRef Blue 
                            ]
  green <- new Gtk.MenuItem [ #label := "Green" 
                            , On #activate $ writeIORef penColorRef Green 
                            ]
  rubber <- new Gtk.MenuItem [ #label := "Rubber" 
                            , On #activate $ writeIORef penColorRef Black 
                             ]
  #add menu white
  #add menu red
  #add menu blue
  #add menu green
  #add menu rubber
  #showAll menu
  return menu

rightClickNotify :: Gdk.EventButton -> Gtk.Menu -> IO Bool
rightClickNotify event menu = do 
  button <- get event #button 
  case button of 
    3 -> do -- Right-click is button 3
         Gtk.menuPopupAtPointer menu Nothing 
         return True 
    _ -> return False


{- Start a surface on the activation of the app -}

initialiseSurface :: IORef (Maybe Ren.Surface) -> IO ()
initialiseSurface surfaceRef = do
    _ <- readIORef surfaceRef >>= \case
      Just _ -> return ()
      Nothing -> do
        s <- Ren.createImageSurface Ren.FormatARGB32 pageWidth pageHeight
        Ren.renderWith s $ backGroundColor >> Ren.paint 
        writeIORef surfaceRef (Just s)
        return ()
    return ()

addPage :: Gtk.Notebook -> IO ()
addPage notebook = do
      -- this is where the right click menu should be initialised
      -- so there is a menu on each drawing area
      -- as well as the IORef app state
      surfaceRef <- newIORef Nothing
      lastPosRef <- newIORef Nothing
      isDrawingRef <- newIORef False
      penColorRef <- newIORef White
      pg <- Gtk.notebookGetNPages notebook
      pageLabel <- new Gtk.Label [ #label := pack $ "Page " ++ show (pg + 1)]
      menu <- rightClickMenu isDrawingRef penColorRef
      drawingArea <- new Gtk.DrawingArea 
        [ #widthRequest := pageWidth
        , #heightRequest := pageHeight
        , On #draw $ handleDraw surfaceRef ?self 
        , On #buttonPressEvent $ \event 
          -> buttonPress event isDrawingRef
        , On #buttonReleaseEvent $ \event 
          -> buttonRelease event isDrawingRef lastPosRef
        , On #motionNotifyEvent $ \event 
          -> motionNotify event surfaceRef lastPosRef isDrawingRef penColorRef ?self
        , On #buttonPressEvent $ \event 
          -> rightClickNotify event menu 
        , On #realize $ realize surfaceRef
        ]
      _ <- Gtk.notebookAppendPage notebook drawingArea (Just pageLabel)
      #addEvents drawingArea 
        [ Gdk.EventMaskButtonPressMask
        , Gdk.EventMaskPointerMotionMask
        , Gdk.EventMaskButtonReleaseMask
        ]
      #showAll notebook

resetNotebookLabels :: Gtk.Notebook -> IO ()
resetNotebookLabels notebook = do
  nPages <- Gtk.notebookGetNPages notebook
  mapM_ (updatePageLabel notebook) [0..fromIntegral nPages - 1]

updatePageLabel :: Gtk.Notebook -> Int -> IO ()
updatePageLabel notebook pageIndex = do
  let newLabel = pack $ "Page " ++ show (pageIndex + 1)
  maybePageWidget <- Gtk.notebookGetNthPage notebook (fromIntegral pageIndex)
  case maybePageWidget of
    Nothing -> return () 
    Just pageWidget -> do
      label <- Gtk.labelNew (Just newLabel)
      Gtk.notebookSetTabLabel notebook pageWidget (Just label)

removePage :: Gtk.Notebook -> IO ()
removePage notebook = do 
  currentPage <- Gtk.notebookGetCurrentPage notebook
  Gtk.notebookRemovePage notebook currentPage
  resetNotebookLabels notebook

{- Initialise and setup drawingArea and Window etc -}

activate :: Gtk.Application -> ApplicationActivateCallback
activate app = do
  notebook <- new Gtk.Notebook []
  #setTabPos notebook Gtk.PositionTypeLeft
  addPage notebook
  scrolledWin <- new Gtk.ScrolledWindow 
    [ #hscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #child := notebook
    ]
  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  win <- new Gtk.ApplicationWindow 
    [ #application := app
    , #title := "Drawing Area"
    , On #destroy (Gtk.widgetDestroy ?self)
    , #child := vbox
    , #defaultWidth := 1200 
    , #defaultHeight := 1000
    ]
  addPageButton <- new Gtk.Button [ #label := "Add Page" 
                                  , On #clicked (addPage notebook)
                                  ]
  removePageButton <- new Gtk.Button [ #label := "Delete Current Page" 
                                     , On #clicked (removePage notebook)
                                     ]

  #packStart vbox addPageButton False False 0
  #packStart vbox removePageButton False False 0
  #packStart vbox scrolledWin True True 0

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
