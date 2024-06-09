{-# LANGUAGE OverloadedStrings #-}

module Questions where
import           BasicSettings 
import           Data.List     (unfoldr)
import           Data.Text     (Text, pack)
import           System.Random (mkStdGen, uniformR)


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

twoDigitNoCarry :: Seed -> Seed -> (Text, Text)
twoDigitNoCarry s1 s2 = (pack $ fstNum ++ " + " ++ sndNum, answer)
  where
    fstRoll = rolls 99 s1
    sndRoll = rolls (9 - (fstRoll `mod` 10)) s2
    answer  = pack . show $ fstRoll + sndRoll
    fstNum  = show fstRoll
    sndNum  = show sndRoll

questionSelector :: Topic -> Seed -> Seed -> (Text, Text)
questionSelector (Number (Addition One)) s1 s2 = singleDigitNoCarry s1 s2
questionSelector (Number (Addition Two)) s1 s2 = twoDigitNoCarry s1 s2
questionSelector _ _ _                         = ("Not", "Completed")
