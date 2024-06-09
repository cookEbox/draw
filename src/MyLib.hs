{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import           BasicSettings
import           Buttons
import           Data.IORef    (newIORef)
import           Data.Text     (pack)
import qualified GI.Gdk        as Gdk
import           GI.Gio
import qualified GI.Gtk        as Gtk
import           Menu

resetNotebookLabels :: Gtk.Notebook -> IO ()
resetNotebookLabels notebook = do
  nPages <- Gtk.notebookGetNPages notebook
  mapM_ (updatePageLabel notebook) [0 .. fromIntegral nPages - 1]

updatePageLabel :: Gtk.Notebook -> Int -> IO ()
updatePageLabel notebook pageIndex = do
  let newLabel = pack $ "Page " ++ show (pageIndex + 1)
  maybePageWidget <- Gtk.notebookGetNthPage notebook (fromIntegral pageIndex)
  case maybePageWidget of
    Nothing -> return ()
    Just pageWidget -> do
      label <- Gtk.labelNew (Just newLabel)
      Gtk.notebookSetTabLabel notebook pageWidget (Just label)

data NewPage = Append | InsertBefore | InsertAfter
  deriving (Eq)

page :: Gtk.Application ->  IO Gtk.DrawingArea
page app = do
  surfaceRef <- newIORef Nothing
  stateRef <- newIORef $ State
                       { getLastPos = Nothing
                       , getIsDrawing = False
                       , getPenColor = White
                       }
  menu <- rightClickMenu stateRef app
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

addPage :: NewPage -> Gtk.Notebook -> Gtk.Application -> IO ()
addPage addOrInsert notebook app = do
  drawingArea <- page app
  pg <- Gtk.notebookGetNPages notebook
  pageLabel <- new Gtk.Label [#label := pack $ "Page " ++ show (pg + 1)]
  currentPosition <- Gtk.notebookGetCurrentPage notebook
  _ <- case addOrInsert of
    Append -> Gtk.notebookAppendPage notebook drawingArea (Just pageLabel)
    InsertAfter -> Gtk.notebookInsertPage notebook drawingArea (Just pageLabel) (currentPosition + 1)
    InsertBefore -> Gtk.notebookInsertPage notebook drawingArea (Just pageLabel) currentPosition
  resetNotebookLabels notebook
  #addEvents drawingArea
    [ Gdk.EventMaskButtonPressMask
    , Gdk.EventMaskPointerMotionMask
    , Gdk.EventMaskButtonReleaseMask
    ]
  #showAll notebook

removePage :: Gtk.Notebook -> IO ()
removePage notebook = do
  currentPage <- Gtk.notebookGetCurrentPage notebook
  Gtk.notebookRemovePage notebook currentPage
  resetNotebookLabels notebook
