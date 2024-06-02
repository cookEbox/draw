{-# LANGUAGE OverloadedStrings #-}

module Questions where
import Data.Text (Text)

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
           | Statistics
  deriving (Show)

singleDigitNoCarry :: Int -> (Text, Text)
singleDigitNoCarry s = undefined

questionSelector :: Topic -> Int -> (Text, Text)
questionSelector (Number (Addition One)) s = singleDigitNoCarry s
questionSelector _ _ = ("Not", "Completed")
