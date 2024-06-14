{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative (Applicative (liftA2))
import           Control.Monad       (when)
import           GI.Gio
import qualified GI.Gtk              as Gtk
import           MyLib               (NewPage (..), addPage, removePage)
import           BasicSettings
import           System.Environment
import           System.Exit


{- Initialise and setup drawingArea and Window etc -}

activate :: Gtk.Application -> ApplicationActivateCallback
activate app = do
  notebook <- new Gtk.Notebook []
  #setTabPos notebook Gtk.PositionTypeLeft
  addPage BasicAndQuestions Append notebook app
  scrolledWin <- new Gtk.ScrolledWindow
    [ #hscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #child := notebook
    ]
  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  hbox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  win <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Drawing Area"
    , On #destroy (Gtk.widgetDestroy ?self)
    , #child := vbox
    , #defaultWidth := 1200
    , #defaultHeight := 1000
    ]
  addPageButton <- new Gtk.Button [ #label := "Add Page"
                                  , On #clicked (addPage BasicAndQuestions Append notebook app)
                                  ]
  insertBeforeButton <- new Gtk.Button [ #label := "Insert Page Before"
                                  , On #clicked (addPage BasicAndQuestions InsertBefore notebook app)
                                  ]
  insertAfterButton <- new Gtk.Button [ #label := "Insert Page After"
                                  , On #clicked (addPage BasicAndQuestions InsertAfter notebook app)
                                  ]
  removePageButton <- new Gtk.Button [ #label := "Delete Current Page"
                                     , On #clicked (removePage notebook)
                                     ]

  #packStart vbox hbox False False 0
  #packStart hbox addPageButton False False 0
  #packStart hbox insertBeforeButton False False 0
  #packStart hbox insertAfterButton False False 0
  #packStart hbox removePageButton False False 0
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
