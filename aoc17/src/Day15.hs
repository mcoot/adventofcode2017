module Day15 (day15Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Numeric
import Data.Char
import Data.List
import Data.Bits

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day15Solution "./data/day15.in"

test :: IO ()
test = runSolver day15Solution "./data/day15test.in"

day15Solution :: Solution GenValues Int
day15Solution = Solution {
    problemName = "Day 15",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = getCount1,
    pt2Sol      = getCount2
}

-- Solution

getCount1 :: GenValues -> Int
getCount1 (a, b) = length $ (filter last16Match) $ (take 40000000) $ zip (genSequence a genAFactor) (genSequence b genBFactor)

getCount2 :: GenValues -> Int
getCount2 (a, b) = length $ (filter last16Match) $ (take 5000000) $ zip (filter (flip divisible 4) $ genSequence a genAFactor) (filter (flip divisible 8) $ genSequence b genBFactor)

divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0

last16Match :: (Int, Int) -> Bool
last16Match (a, b) = (mask16 a) == (mask16 b)

genSequence :: Int -> Int-> [Int]
genSequence seed f = unfoldr (Just . duplPair . (multAndRem f genDivisor)) seed

duplPair :: a -> (a, a)
duplPair x = (x, x)

multAndRem :: Int -> Int -> Int -> Int
multAndRem f d prev = (prev * f) `rem` d

-- Data

type GenValues = (Int, Int)

genAFactor :: Int
genAFactor = 16807

genBFactor :: Int
genBFactor = 48271

genDivisor :: Int
genDivisor = 2147483647

mask32 :: Int -> Int
mask32 = (.&.) (2^32 - 1)

mask31 :: Int -> Int
mask31 = (.&.) genDivisor

mask16 :: Int -> Int
mask16 = (.&.) 65535

-- Input parsing

inputParser :: Parser GenValues
inputParser = (,) <$> int <*> (endOfLine *> int)

showBinary :: (Integral a, Show a) => a -> String
showBinary a = showIntAtBase 2 intToDigit a ""