{-# LANGUAGE OverloadedStrings #-}
module BasicSettings where

import qualified GI.Cairo.Render           as Ren

{- Basic settings -}

question :: String
question = "1 + 1"

data Color = Default | White | Blue | Red | Green | Black deriving (Show, Eq)

backGroundColor :: Ren.Render ()
backGroundColor = Ren.setSourceRGB 0 0 0

penColor :: Color -> Ren.Render ()
penColor Default = Ren.setSourceRGB 255 255 255
penColor White = Ren.setSourceRGB 255 255 255
penColor Blue = Ren.setSourceRGB 0 0 255
penColor Red = Ren.setSourceRGB 255 0 0 
penColor Green = Ren.setSourceRGB 0 255 0 
penColor Black = Ren.setSourceRGB 0 0 0 

pageWidth, pageHeight :: Integral a => a 
pageWidth = 2200
pageHeight = 1300

data State = State 
  { getLastPos :: Maybe (Double, Double)
  , getIsDrawing :: Bool
  , getPenColor :: Color
  }
