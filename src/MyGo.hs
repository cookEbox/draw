{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Cairo as GICairo
import GI.Gio
import Graphics.Rendering.Cairo 
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)

import Control.Applicative 
import Control.Monad
import Control.Monad.Trans.Reader
import System.Exit
import System.Environment
-- import qualified GI.Gdk as Gdk
-- import Data.GI.Base
import GI.Cairo.Render.Connector as Con
import GI.Cairo.Render as Ren
-- import Control.Monad.IO.Class

import Data.GI.Base
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

drawLines :: GICairo.Context -> IO ()
drawLines context = do
    Con.renderWithContext (Ren.setSourceRGB 0 0 0) context
    Con.renderWithContext (Ren.setLineWidth 1) context

getEventButtonCoords :: Gtk.EventButton -> IO (Double, Double)
getEventButtonCoords eventButton = do
    x <- Gtk.get eventButton #x
    y <- Gtk.get eventButton #y
    return (x, y)

getEventMotionCoords :: Gtk.EventMotion -> IO (Double, Double)
getEventMotionCoords eventMotion = do
    x <- Gtk.get eventMotion #x
    y <- Gtk.get eventMotion #y
    return (x, y)

main :: IO ()
main = do
    Gtk.init Nothing

    win <- new Gtk.Window [ #title := "Drawing App" ]
    on win #destroy Gtk.mainQuit

    canvas <- Gtk.drawingAreaNew
    #add win canvas

    -- Connect the draw signal to our drawing function
    on canvas #draw $ \context -> do
        drawLines context
        return False

    -- Handle mouse events for drawing
    drawingRef <- newIORef False
    lastPosRef <- newIORef (0, 0)

    on canvas #buttonPressEvent $ \eventButton -> do
        button <- get eventButton #button
        when (button == 1) $ do -- Left mouse button
            writeIORef drawingRef True
            (x, y) <- Gtk.getEventButtonCoords eventButton
            writeIORef lastPosRef (x, y)
        return False

    on canvas #buttonReleaseEvent $ \eventButton -> do
        button <- get eventButton #button
        when (button == 1) $ do -- Left mouse button
            writeIORef drawingRef False
        return False

    on canvas #motionNotifyEvent $ \eventMotion -> do
        drawing <- readIORef drawingRef
        when drawing $ do
            (x, y) <- Gtk.getEventMotionCoords eventMotion
            lastPos <- readIORef lastPosRef
            writeIORef lastPosRef (x, y)
            -- Trigger a redraw of the canvas
            Gtk.widgetQueueDraw canvas
        return False

    -- Show the window
    #showAll win

    Gtk.main

