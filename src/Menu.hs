{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Menu where

import           BasicSettings
import           Data.IORef    (IORef, readIORef, writeIORef, newIORef)
import           Data.Text     (Text, pack)
import           Questions
import           System.Random (randomRIO)
import           Buttons
import qualified GI.Cairo                  as Cairo
import qualified GI.Cairo.Render           as Ren
import qualified GI.Cairo.Render.Connector as Con
import qualified GI.Gdk                    as Gdk
import           GI.Gio
import qualified GI.Gtk                    as Gtk
import Control.Monad (void)

page :: RCMenus -> Gtk.Application ->  IO Gtk.DrawingArea
page rcm app = do
  surfaceRef <- newIORef Nothing
  stateRef <- newIORef $ State
                       { getLastPos = Nothing
                       , getIsDrawing = False
                       , getPenColor = White
                       }
  menu <- rightClickMenu rcm stateRef app
  new Gtk.DrawingArea
    [ #widthRequest := pageWidth
    , #heightRequest := pageHeight
    , On #draw $ handleDraw surfaceRef ?self
    , On #buttonPressEvent $ \event ->
        buttonPress event stateRef
    , On #buttonReleaseEvent $ \event ->
        buttonRelease event stateRef
    , On #motionNotifyEvent $ \event ->
        motionNotify event surfaceRef stateRef ?self
    , On #buttonPressEvent $ \event ->
        rightClickNotify event menu
    , On #realize $ realize surfaceRef
    ]

createFloatingNotepad :: Gtk.Application -> Topic -> IO ()
createFloatingNotepad app topic = do
  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  floatingWindow <- new Gtk.Window [ #title := "Question Window"
                                   , #child := vbox
                                   ]
  #setTransientFor floatingWindow =<< Gtk.applicationGetActiveWindow app
  #setDefaultSize floatingWindow 300 200
  notebook <- new Gtk.Notebook []
  printQuestion <- new Gtk.Button [ #label := "Print Questions"
                                  ]
  #packStart vbox printQuestion False False 0
  #packStart vbox notebook True True 0
  -- Create several tabs with drawing areas
  s1 <- randomRIO (0 :: Int, 5000)
  s2 <- randomRIO (0 :: Int, 5000)
  let (question, answer) = questionSelector topic s1 s2
  mapM_ (addPageTab notebook app) [("Questions", question), ("Answers", answer)]
  #showAll floatingWindow

addPageTab :: Gtk.Notebook -> Gtk.Application -> (Text, Text) -> IO ()
addPageTab notebook app (label, qa) = do
  drawingArea <- page Basic app
  void $ Gtk.onWidgetDraw drawingArea $ \context -> do 
    putStrLn "Draw callback triggered"
    Con.renderWithContext (do 
      Ren.setSourceRGB 1 1 1 
      Ren.selectFontFace (pack "Sans") Ren.FontSlantNormal Ren.FontWeightNormal
      Ren.setFontSize 40
      Ren.moveTo 10 50
      Ren.showText qa) context
    return False
  labelWidget <- new Gtk.Label [#label := label]
  _ <- Gtk.notebookAppendPage notebook drawingArea (Just labelWidget)
  #addEvents drawingArea
    [ Gdk.EventMaskButtonPressMask
    , Gdk.EventMaskPointerMotionMask
    , Gdk.EventMaskButtonReleaseMask
    ]
  #showAll drawingArea

-- addPageTab :: Gtk.Notebook -> (Text, Text) -> IO ()
-- addPageTab notebook (label, qa) = do
--   pg <- Gtk.labelNew (Just qa)
--   labelWidget <- new Gtk.Label [#label := label]
--   _ <- Gtk.notebookAppendPage notebook pg (Just labelWidget)
--   #showAll pg

rightClickMenuQuestions :: Gtk.Application -> Gtk.Menu -> IO ()
rightClickMenuQuestions app menu = do 
  topicMenu <- new Gtk.Menu []
  topics <- new Gtk.MenuItem [ #label := "Topics" ]
  numberMenu <- new Gtk.Menu []
  number <- new Gtk.MenuItem [ #label := "Number" ]
  addMenu <- new Gtk.Menu []
  geometry <- new Gtk.MenuItem [ #label := "Geometry" ]
  geoMenu <- new Gtk.Menu []
  addition <- new Gtk.MenuItem [ #label := "Add Questions" ]
  subMenu <- new Gtk.Menu []
  subtraction <- new Gtk.MenuItem [ #label := "Subtraction Questions" ]
  areaMenu <- new Gtk.Menu []
  area <- new Gtk.MenuItem [ #label := "Area Questions" ]
  add1 <- new Gtk.MenuItem [ #label := "Add Level 1"
                           , On #activate $ createFloatingNotepad app (Number (Addition One))
                           ]
  add2 <- new Gtk.MenuItem [ #label := "Add Level 2"
                           , On #activate $ createFloatingNotepad app (Number (Addition Two))
                           ]
  sub1 <- new Gtk.MenuItem [ #label := "Subtract Level 1"
                           , On #activate $ createFloatingNotepad app (Number (Subtraction One))
                           ]
  area1 <- new Gtk.MenuItem [ #label := "Area Level 1"
                            , On #activate $ createFloatingNotepad app (Geometry (Area One))
                            ]
  Gtk.menuItemSetSubmenu topics (Just topicMenu)
  Gtk.menuItemSetSubmenu number (Just numberMenu)
  Gtk.menuItemSetSubmenu geometry (Just geoMenu)
  Gtk.menuItemSetSubmenu area (Just areaMenu)
  Gtk.menuItemSetSubmenu addition (Just addMenu)
  Gtk.menuItemSetSubmenu subtraction (Just subMenu)
  #add menu topics
  #add topicMenu number
  #add topicMenu geometry
  #add numberMenu addition
  #add addMenu add1
  #add addMenu add2
  #add numberMenu subtraction
  #add subMenu sub1
  #add geoMenu area
  #add areaMenu area1

rightClickMenu :: RCMenus -> IORef State -> Gtk.Application -> IO Gtk.Menu
rightClickMenu rcm stateRef app = do
  state <- readIORef stateRef
  menu <- new Gtk.Menu [ On #hide $ writeIORef stateRef (newDrawingState state) ]
  white <- new Gtk.MenuItem
    [ #label := "White"
    , On #activate $ writeIORef stateRef (newPenColor state White)
    ]
  red <- new Gtk.MenuItem
    [ #label := "Red"
    , On #activate $ writeIORef stateRef (newPenColor state Red)
    ]
  blue <- new Gtk.MenuItem
    [ #label := "Blue"
    , On #activate $ writeIORef stateRef (newPenColor state Blue)
    ]
  green <- new Gtk.MenuItem
    [ #label := "Green"
    , On #activate $ writeIORef stateRef (newPenColor state Green)
    ]
  rubber <- new Gtk.MenuItem
    [ #label := "Rubber"
    , On #activate $ writeIORef stateRef (newPenColor state Default)
    ]
  #add menu white
  #add menu red
  #add menu blue
  #add menu green
  #add menu rubber
  if rcm == BasicAndQuestions then do
    rightClickMenuQuestions app menu
  else do
    return ()

  #showAll menu
  return menu
    where newDrawingState st = st { getIsDrawing = False }
          newPenColor st cl = st { getPenColor = cl }

