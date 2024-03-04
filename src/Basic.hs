{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE ImplicitParams    #-}
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
-- import GI.Cairo.Render.Connector as Cairo
-- import Control.Monad.IO.Class

-- drawCb :: Gtk.DrawingArea -> w -> IO Bool
-- drawCb context = do 
  -- liftIO $ GICairo.withManagedPtr context $ \ptr -> do 
  --     let cairoContext = Cairo $ castPtr ptr
  --     runReaderT (runRender drawingCommands) cairoContext
  -- pure True

drawingCommands :: Render ()
drawingCommands = do 
  setSourceRGB 0 0 0 
  moveTo 10 10 
  lineTo 200 200 
  stroke 
  

activate :: Gtk.Application -> ApplicationActivateCallback 
activate app = do 
  drawingArea <- new Gtk.DrawingArea [ #heightRequest := 100
                                     , #widthRequest := 100
                                     -- , On #draw (drawCb ?self)
                                     ]

  frame <- new Gtk.Frame [ #child := drawingArea ]

  win <- new Gtk.ApplicationWindow [ #application := app 
                                   , #title := "Drawing Area"
                                   , On #destroy (Gtk.widgetDestroy ?self) 
                                   , #child := frame
                                   ]

  -- Just window <- Gtk.widgetGetWindow win 
  -- surf <- Gdk.cairoCreate window

  _ <- on drawingArea #draw $ \context -> do 
    liftIO $ GICairo.withManagedPtr context $ \ptr -> do 
        let cairoContext = Cairo $ castPtr ptr
        runReaderT (runRender drawingCommands) cairoContext
    pure True
    

  #showAll win

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "org.gtk.example"
                             , On #activate (activate ?self)
                             ] 

  argv <- liftA2 (:) getProgName getArgs 
  stas <- #run app $ Just argv

  when (stas /= 0) $
    exitWith $ ExitFailure (fromIntegral stas)
