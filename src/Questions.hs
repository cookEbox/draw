{-# LANGUAGE OverloadedStrings #-}

module Questions where
import           BasicSettings 
import           Data.List     (unfoldr)
import           System.Random (mkStdGen, uniformR)

rolls :: Seed -> Seed -> Int
rolls n = head . unfoldr (Just . uniformR (0, n)) . mkStdGen

singleDigitAddNoCarry :: Seed -> Seed -> (String, String)
singleDigitAddNoCarry s1 s2 = (fstNum ++ " + " ++ sndNum, answer)
  where
    fstRoll = rolls 9 s1
    sndRoll = rolls (9 - fstRoll) s2
    answer  = show $ fstRoll + sndRoll
    fstNum  = show fstRoll
    sndNum  = show sndRoll

twoDigitAddNoCarry :: Seed -> Seed -> (String, String)
twoDigitAddNoCarry s1 s2 = (fstNum ++ " + " ++ sndNum, answer)
  where
    fstRoll = rolls 99 s1
    sndRoll = rolls (9 - (fstRoll `mod` 10)) s2
    answer  = show $ fstRoll + sndRoll
    fstNum  = show fstRoll
    sndNum  = show sndRoll

singleDigitSubNoCarry :: Seed -> Seed -> (String, String)
singleDigitSubNoCarry s1 s2 = (fstNum ++ " - " ++ sndNum, answer)
  where
    fstRoll = rolls 9 s1
    sndRoll = rolls fstRoll s2
    answer  = show $ fstRoll - sndRoll
    fstNum  = show fstRoll
    sndNum  = show sndRoll

areaOfRectangle1 :: Seed -> (String, String)
areaOfRectangle1 s1 = (question, answer ++ " cm^2")
  where
    question = "What is the area of a rectangle with height 10 cm and width " ++ fstNum ++ " cm"
    fstRoll = rolls 9 s1
    answer  = show $ fstRoll * 10
    fstNum  = show fstRoll

questionSelector :: Topic -> Seed -> Seed -> (String, String)
questionSelector (Number (Addition One)) s1 s2 = singleDigitAddNoCarry s1 s2
questionSelector (Number (Subtraction One)) s1 s2 = singleDigitSubNoCarry s1 s2
questionSelector (Number (Addition Two)) s1 s2 = twoDigitAddNoCarry s1 s2
questionSelector (Geometry (Area One)) s1 _ = areaOfRectangle1 s1 
questionSelector _ _ _                         = ("Not", "Completed")
