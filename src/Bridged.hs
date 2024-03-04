{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE ImplicitParams    #-}
module Bridged where

import qualified GI.Gtk as Gtk
import qualified GI.Cairo as GICairo
import GI.Gio
import Graphics.Rendering.Cairo as C
import Unsafe.Coerce
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import GI.Cairo.Render.Connector (renderWithContext)
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

-- Drawing commands using Graphics.Rendering.Cairo
drawingCommands :: Render ()
drawingCommands = do
  setSourceRGB 1 0 0 -- Red
  moveTo 10 10
  lineTo 100 100
  stroke

drawCb :: Gtk.DrawingArea -> GICairo.Context -> IO Bool
drawCb _ context = liftIO $ do
  -- Bridge the GI_Cairo.Context to a Graphics.Rendering.Cairo context and draw
  renderWithContext context drawingCommands
  return True
  
activate :: Gtk.Application -> ApplicationActivateCallback 
activate app = do 
  drawingArea <- new Gtk.DrawingArea [ #heightRequest := 100
                                     , #widthRequest := 100
                                     , On #draw (drawCb ?self)
                                     ]

  frame <- new Gtk.Frame [ #child := drawingArea ]

  win <- new Gtk.ApplicationWindow [ #application := app 
                                   , #title := "Drawing Area"
                                   , On #destroy (Gtk.widgetDestroy ?self) 
                                   , #child := frame
                                   ]

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
