module Day10 (day10Solution, getHash) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Control.Monad
import Data.Char
import Data.List
import Data.Bits

import Data.Vector (Vector)
import qualified Data.Vector as V

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day10Solution "./data/day10.in"

test :: IO ()
test = runSolver day10Solution "./data/day10test.in"

day10Solution :: Solution String String
day10Solution = Solution {
    problemName = "Day 10",
    solParser   = Just . id,
    pt1Sol      = execPart1,
    pt2Sol      = execPart2
}

-- Part 1

execPart1 :: String -> String
execPart1 str = case parseInputMaybe part1Parser str of
                Nothing  -> "[Parsing failed]"
                Just lst -> show $ multFirstTwo $ fstTriple $ getRound (V.fromList [0..255]) lst 0 0

execPart2 :: String -> String
execPart2 str = case parseInputMaybe part2Parser str of
                    Nothing -> "[Parsing failed]"
                    Just r -> calcHash (V.fromList [0..255]) (r ++ [17, 31, 73, 47, 23])

fstTriple :: (a, b, c) -> a
fstTriple (x, _, _) = x

getRound :: Vector Integer -> [Integer] -> Integer -> Integer -> ([Integer], Integer, Integer)
getRound vec lengths pos skipSize = (V.toList res, endPos, endSkipSize)
    where
        (res, endPos, endSkipSize) = performRound vec lengths pos skipSize

multFirstTwo :: [Integer] -> Integer
multFirstTwo (x:y:_) = x * y

performRound :: Vector Integer -> [Integer] -> Integer -> Integer -> (Vector Integer, Integer, Integer)
performRound vec lengths pos skipSize = performRoundRecurse vec lengths pos skipSize

performRoundRecurse :: Vector Integer -> [Integer] -> Integer -> Integer -> (Vector Integer, Integer, Integer)
performRoundRecurse vec [] idx skipSize = (vec, idx, skipSize)
performRoundRecurse vec (x:xs) idx skipSize = performRoundRecurse newVec xs ((idx + x + skipSize) `mod` (toInteger $ length vec)) (skipSize + 1)
    where newVec = performHashStep vec x idx

performHashStep :: Vector Integer -> Integer -> Integer -> Vector Integer
performHashStep vec x idx
    | (fromIntegral $ idx + x) <= length vec = hashStepNoWrap vec x idx
    | otherwise                              = hashStepWrap vec x idx

hashStepNoWrap :: Vector Integer -> Integer -> Integer -> Vector Integer
hashStepNoWrap vec x idx = leftRest V.++ revd V.++ rightRest
    where 
        revd = V.reverse $ getCircularSlice vec idx x
        leftRest  = V.slice 0 (fromIntegral $ idx) vec
        rightRest = V.slice (fromIntegral $ idx + x) (length vec - (fromIntegral $ idx + x)) vec

hashStepWrap :: Vector Integer -> Integer -> Integer -> Vector Integer
hashStepWrap vec x idx = wrappedRevd V.++ rest V.++ unwrappedRevd
    where
        revd = V.reverse $ getCircularSlice vec idx x
        rest = V.slice ((fromIntegral $ idx + x) `mod` (length vec)) ((length vec) - (fromIntegral $ x)) vec
        wrappedRevdIdx = getCircularCutPos vec idx x
        wrappedRevd = V.slice (fromIntegral $ wrappedRevdIdx) (length revd - (fromIntegral $ wrappedRevdIdx)) revd
        unwrappedRevd = V.slice 0 (fromIntegral $ wrappedRevdIdx) revd

getCircularCutPos :: Vector Integer -> Integer -> Integer -> Integer
getCircularCutPos vec idx n
    | (fromIntegral $ idx + n) <= length vec  = 0
    | otherwise                               = toInteger $ length vec - (fromIntegral idx)

getCircularSlice :: Vector Integer -> Integer -> Integer -> Vector Integer
getCircularSlice vec idx n 
    | (fromIntegral $ idx + n) <= length vec  = V.slice (fromIntegral idx) (fromIntegral n) vec
    | otherwise                               = mainPart V.++ wrappedPart
    where 
        mainPart    = V.slice (fromIntegral idx) (length vec - (fromIntegral idx)) vec
        wrappedPart = V.slice 0 ((fromIntegral n) - (length vec - (fromIntegral idx))) vec

-- Part 2

getHash :: String -> String
getHash str = case parseInputMaybe part2Parser str of
                    Nothing -> ""
                    Just r -> calcHash (V.fromList [0..255]) (r ++ [17, 31, 73, 47, 23])

calcHash :: Vector Integer -> [Integer] -> String
calcHash vec lengths = hexifyHash $ densifyHash $ V.toList $ roundRes
    where roundRes = getHashRecurse vec lengths (0, 0) 64

getHashRecurse :: Vector Integer -> [Integer] -> (Integer, Integer) -> Integer -> Vector Integer
getHashRecurse vec lengths (pos, skp) iterLeft
    | iterLeft <= 0 = vec
    | otherwise     = getHashRecurse (V.fromList newVec) lengths (newPos, newSkp) (iterLeft - 1)
    where
        (newVec, newPos, newSkp) = getRound vec lengths pos skp

hexifyHash :: [Integer] -> String
hexifyHash lst = foldl (\accum cur -> accum ++ (numToHexChars cur)) "" lst

numToHexChars :: Integer -> String
numToHexChars n = (intToDigit $ fromIntegral highHalf) : (intToDigit $ fromIntegral lowHalf) : []
    where lowHalf  = n .&. 15
          highHalf = (fromIntegral n :: Int) `shiftR` (4 :: Int)

densifyHash :: [Integer] -> [Integer]
densifyHash lst = densifyHashRecurse (V.fromList lst) [] 0

densifyHashRecurse :: Vector Integer -> [Integer] -> Integer -> [Integer]
densifyHashRecurse orig inprog idx 
    | idx >= (toInteger $ length orig) = inprog
    | otherwise     = densifyHashRecurse orig (inprog ++ [(foldr xor 0 curSlice)]) (idx + 16)
    where 
        curSlice = V.toList $ V.slice (fromIntegral idx) 16 orig

-- Input Parsing

part1Parser :: Parser [Integer]
part1Parser = int `sepBy` char ','

part2Parser :: Parser [Integer]
part2Parser = many (anyChar >>= (return . toInteger . ord))