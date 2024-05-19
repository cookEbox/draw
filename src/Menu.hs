{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Menu where

import           BasicSettings
import           Data.IORef    (IORef, writeIORef)
import           Data.Text     (Text)
import           GI.Gio
import qualified GI.Gtk        as Gtk

createFloatingNotepad :: Gtk.Application -> IO ()
createFloatingNotepad app = do
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
  mapM_ (addDrawingAreaTab notebook) ["Questions", "Answers"]
  #showAll floatingWindow

addDrawingAreaTab :: Gtk.Notebook -> Text -> IO ()
addDrawingAreaTab notebook label = do
  drawingArea <- new Gtk.DrawingArea []
  labelWidget <- new Gtk.Label [#label := label]
  _ <- Gtk.notebookAppendPage notebook drawingArea (Just labelWidget)
  #showAll drawingArea

rightClickMenu :: IORef Bool -> IORef Color -> Gtk.Application -> IO Gtk.Menu
rightClickMenu isDrawingRef penColorRef app = do
  menu <- new Gtk.Menu [ On #hide $ writeIORef isDrawingRef False]
  white <- new Gtk.MenuItem     [ #label := "White"
                                , On #activate $ writeIORef penColorRef White
                                ]
  red <- new Gtk.MenuItem       [ #label := "Red"
                                , On #activate $ writeIORef penColorRef Red
                                ]
  blue <- new Gtk.MenuItem      [ #label := "Blue"
                                , On #activate $ writeIORef penColorRef Blue
                                ]
  green <- new Gtk.MenuItem     [ #label := "Green"
                                , On #activate $ writeIORef penColorRef Green
                                ]
  rubber <- new Gtk.MenuItem    [ #label := "Rubber"
                                , On #activate $ writeIORef penColorRef Black
                                ]
  questions <- new Gtk.MenuItem [ #label := "Questions"
                                , On #activate $ createFloatingNotepad app
                                ]
  #add menu white
  #add menu red
  #add menu blue
  #add menu green
  #add menu rubber
  #add menu questions
  #showAll menu
  return menu

