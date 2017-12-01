module Day1 where

import Data.Char

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

main :: IO ()
main = do
    inputData <- readFile "./data/day1.in"
    putStrLn $ "The inverse captcha result for part 1 is " ++ (show $ inverseCaptcha1 inputData)
    putStrLn $ "The inverse captcha result for part 2 is " ++ (show $ inverseCaptcha2 inputData)
