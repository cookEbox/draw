{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import           BasicSettings
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

addPage :: RCMenus -> NewPage -> Gtk.Notebook -> Gtk.Application -> IO ()
addPage rcm addOrInsert notebook app = do
  drawingArea <- page rcm app
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
