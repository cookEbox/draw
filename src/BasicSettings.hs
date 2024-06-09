-- {-# LANGUAGE OverloadedStrings #-}
module BasicSettings where

import qualified GI.Cairo.Render as Ren

type Seed = Int

data Level = One | Two | Three
  deriving (Show)

data NumType = Addition Level
             | Subtraction Level
  deriving (Show)

data GeoType = Polygon Level
             | Circle Level
  deriving (Show)

data AlgType = Collect Level
             | Solve Level
  deriving (Show)

data StaType = Average Level
             | Graphs Level
  deriving (Show)

data Topic = Number NumType
           | Geometry GeoType
           | Algebra AlgType
           | Statistics StaType
  deriving (Show)

data Color = Default | White | Blue | Red | Green | Black
  deriving (Eq, Show)

backGroundColor :: Ren.Render ()
backGroundColor = Ren.setSourceRGB 0 0 0

penColor :: Color -> Ren.Render ()
penColor Default = Ren.setSourceRGB 255 255 255
penColor White   = Ren.setSourceRGB 255 255 255
penColor Blue    = Ren.setSourceRGB 0 0 255
penColor Red     = Ren.setSourceRGB 255 0 0
penColor Green   = Ren.setSourceRGB 0 255 0
penColor Black   = Ren.setSourceRGB 0 0 0

pageWidth, pageHeight :: Integral a => a
pageWidth = 2200
pageHeight = 1300

data State = State
               { getLastPos   :: Maybe (Double, Double)
               , getIsDrawing :: Bool
               , getPenColor  :: Color
               }

data RCMenus = Basic | BasicAndQuestions
  deriving (Eq)

