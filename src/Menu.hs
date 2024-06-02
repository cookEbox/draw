{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Menu where

import           BasicSettings
import           Data.IORef    (IORef, writeIORef, readIORef)
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
  mapM_ (addPageTab notebook) [("Questions", question), ("Answers", answer)]
  #showAll floatingWindow

addPageTab :: Gtk.Notebook -> (Text, Text) -> IO ()
addPageTab notebook (label, qa) = do
  page <- Gtk.labelNew (Just qa)
  labelWidget <- new Gtk.Label [#label := label]
  _ <- Gtk.notebookAppendPage notebook page (Just labelWidget)
  #showAll page

rightClickMenu :: IORef State -> Gtk.Application -> IO Gtk.Menu
rightClickMenu stateRef app = do
  state <- readIORef stateRef
  menu <- new Gtk.Menu [ On #hide $ writeIORef stateRef (newDrawingState state) ]
  white <- new Gtk.MenuItem     [ #label := "White"
                                , On #activate $ writeIORef stateRef (newPenColor state White)
                                ]
  red <- new Gtk.MenuItem       [ #label := "Red"
                                , On #activate $ writeIORef stateRef (newPenColor state Red)
                                ]
  blue <- new Gtk.MenuItem      [ #label := "Blue"
                                , On #activate $ writeIORef stateRef (newPenColor state Blue)
                                ]
  green <- new Gtk.MenuItem     [ #label := "Green"
                                , On #activate $ writeIORef stateRef (newPenColor state Green)
                                ]
  rubber <- new Gtk.MenuItem    [ #label := "Rubber"
                                , On #activate $ writeIORef stateRef (newPenColor state Black)
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
    where newDrawingState st = st { getIsDrawing = False }
          newPenColor st cl = st { getPenColor = cl }

