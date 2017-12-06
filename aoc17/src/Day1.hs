module Day1 (day1Solution) where

import Data.Char

import Solver

main :: IO ()
main = runSolver day1Solution "./data/day1.in"

day1Solution :: Solution String Int
day1Solution = Solution {
    problemName = "Day 1",
    solParser   = Just . id,
    pt1Sol      = inverseCaptcha1,
    pt2Sol      = inverseCaptcha2
}

isFurtherDigitTheSame :: String -> Int -> Int -> Bool
isFurtherDigitTheSame str idx count = (str !! idx == str !! (mod (idx + count) (length str)))

isNextDigitTheSame :: String -> Int -> Bool
isNextDigitTheSame str idx = isFurtherDigitTheSame str idx 1

inverseCaptcha :: String -> Int -> Int
inverseCaptcha str count = sum $ fmap digitToInt $ (\idx -> if isFurtherDigitTheSame str idx count then str !! idx else '0') <$> [0..(length str - 1)]

inverseCaptcha1 :: String -> Int
inverseCaptcha1 str = inverseCaptcha str 1

inverseCaptcha2 :: String -> Int
inverseCaptcha2 str = inverseCaptcha str $ length str `quot` 2