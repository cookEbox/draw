{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module MyLib where

import           BasicSettings
import           Buttons
import           Data.IORef      (newIORef)
import           Data.Text       (pack)
import qualified GI.Gdk          as Gdk
import           GI.Gio
import qualified GI.Gtk          as Gtk
import           Menu
import           SharedFunctions

data NewPage = Append | InsertBefore | InsertAfter deriving Eq

addPage :: NewPage -> Gtk.Notebook -> Gtk.Application -> IO ()
addPage addOrInsert notebook app = do
      surfaceRef <- newIORef Nothing
      lastPosRef <- newIORef Nothing
      isDrawingRef <- newIORef False
      penColorRef <- newIORef White
      pg <- Gtk.notebookGetNPages notebook
      pageLabel <- new Gtk.Label [ #label := pack $ "Page " ++ show (pg + 1)]
      currentPosition <- Gtk.notebookGetCurrentPage notebook
      menu <- rightClickMenu isDrawingRef penColorRef app
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
