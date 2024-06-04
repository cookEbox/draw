{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Menu where

import           BasicSettings
import           Data.IORef    (IORef, readIORef, writeIORef)
import           Data.Text     (Text)
import           GI.Gio
import qualified GI.Gtk        as Gtk
import           Questions
import           System.Random (randomRIO)

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
  s1 <- randomRIO (0 :: Int, 5000)
  s2 <- randomRIO (0 :: Int, 5000)
  let (question, answer) = questionSelector (Number (Addition One)) s1 s2
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
  questionMenu <- new Gtk.Menu []
  questions <- new Gtk.MenuItem [ #label := "Questions" ]
  addMenu <- new Gtk.Menu []
  addition <- new Gtk.MenuItem [ #label := "Add Questions" ]
  add1 <- new Gtk.MenuItem [ #label := "Add Level 1"
                           , On #activate $ createFloatingNotepad app
                           ]
  Gtk.menuItemSetSubmenu questions (Just questionMenu)
  Gtk.menuItemSetSubmenu addition (Just addMenu)
  #add menu white
  #add menu red
  #add menu blue
  #add menu green
  #add menu rubber
  #add menu questions
  #add questionMenu addition
  #add addMenu add1
  #showAll menu
  return menu
    where newDrawingState st = st { getIsDrawing = False }
          newPenColor st cl = st { getPenColor = cl }

