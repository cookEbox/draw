{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE ImplicitParams    #-}
module BrokenExample where

import qualified GI.Gtk as Gtk
import qualified GI.Cairo as GICairo
import GI.Gio
import Graphics.Rendering.Cairo as C
import Unsafe.Coerce
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

createMySurface :: IO C.Surface 
createMySurface = C.createImageSurface C.FormatARGB32 100 100 >>= \surface -> do 
  C.renderWith surface $ do 
    C.setSourceRGB 1 0 0 
    C.paint 
  return surface

drawOnGtkDrawingArea :: GICairo.Context -> IO ()
drawOnGtkDrawingArea cairoContext = do
  -- Assuming 'createMySurface' creates the surface you want to draw
  mySurface <- createMySurface
  -- Get the width and height of the surface to set as the source
  width <- C.imageSurfaceGetWidth mySurface
  height <- C.imageSurfaceGetHeight mySurface
  -- Convert GI_Cairo.Context to Cairo.Render to draw on it
  GICairo.withCairo $ \context -> do
    C.renderWith context $ do
      -- Set the created surface as the source for drawing operations
      C.setSourceSurface mySurface 0 0
      -- Paint the source surface onto the drawing area
      C.paint
      -- Optionally, do more drawing here with Cairo functions

drawCb :: Gtk.DrawingArea -> GICairo.Context -> IO Bool
drawCb _widget context = do 
  mySurface <- createMySurface
  C.renderWith context $ do 
    C.setSourceSurface mySurface 0 0 
    C.paint 
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
