{-# LANGUAGE OverloadedStrings #-}

module Questions where
import           Data.List     (unfoldr)
import           Data.Text     (Text, pack)
import           System.Random (mkStdGen, uniformR)

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

rolls :: Seed -> Seed -> Int
rolls n = head . unfoldr (Just . uniformR (0, n)) . mkStdGen

singleDigitNoCarry :: Seed -> Seed -> (Text, Text)
singleDigitNoCarry s1 s2 = (pack $ fstNum ++ " + " ++ sndNum, answer)
  where
    fstRoll = rolls 9 s1
    sndRoll = rolls (9 - fstRoll) s2
    answer  = pack . show $ fstRoll + sndRoll
    fstNum  = show fstRoll
    sndNum  = show sndRoll

questionSelector :: Topic -> Seed -> Seed -> (Text, Text)
questionSelector (Number (Addition One)) s1 s2 = singleDigitNoCarry s1 s2
questionSelector _ _ _                         = ("Not", "Completed")
