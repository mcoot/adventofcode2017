module Day14 (day14Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Numeric
import Data.Word
import Data.Char
import Data.List

import Data.Set (Set)
import qualified Data.Set as S

import Data.Bits
import Data.DoubleWord

import Debug.Trace

import Day10 (getHash)
import Solver

main :: IO ()
main = runSolver day14Solution "./data/day14.in"

test :: IO ()
test = runSolver day14Solution "./data/day14test.in"

day14Solution :: Solution [Word128] Int
day14Solution = Solution {
    problemName = "Day 14",
    solParser   = Just . genInput,
    pt1Sol      = numUsedCells,
    pt2Sol      = getNumRegions . getUsedPoints
}

-- Solution

numUsedCells :: [Word128] -> Int
numUsedCells = sum . (fmap popCount)

getNumRegions :: Set (Int, Int) -> Int
getNumRegions used 
    | S.null used = 0
    | otherwise   = 1 + getNumRegions (removeRegion used (S.elemAt 0 used))

removeRegion :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
removeRegion used cell = used `S.difference` (findRegion used S.empty cell)

findRegion :: Set (Int, Int) -> Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
findRegion used found cell
    | not $ S.member cell used = found
    | otherwise                = foldr (\nCell accum -> 
                                                if not (S.member nCell found) && S.member nCell used then 
                                                    (findRegion used accum nCell) 
                                                else 
                                                    accum)
                                        (S.insert cell found)
                                        (neighbourCells cell)

neighbourCells :: (Int, Int) -> [(Int, Int)]
neighbourCells (x, y) = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

getUsedPoints :: [Word128] -> Set (Int, Int)
getUsedPoints = S.fromList . concat . (fmap (\(i, vals) -> ((,) i) <$> vals)) . zip [0..127] . fmap getRowPoints

getRowPoints :: Word128 -> [Int]
getRowPoints row = foldr (\i accum -> if testBit row i then i : accum else accum) [] [0..127]

-- Input Parsing

showBinary :: (Integral a, Show a) => a -> String
showBinary a = showIntAtBase 2 intToDigit a ""

genInput :: String -> [Word128]
genInput inp = (\i -> hexToBits $ getHash $ inp ++ "-" ++ (show i)) <$> [0..127]

binaryToBits :: String -> Word128
binaryToBits [] = zeroBits
binaryToBits (c:cs) = (curDigit `shiftL` shiftAmount) .|. binaryToBits cs
    where
        curDigit = if c == '1' then 1 else 0
        shiftAmount = length cs

hexToBits :: String -> Word128
hexToBits [] = zeroBits
hexToBits (c:cs) = (curDigit `shiftL` shiftAmount) .|. hexToBits cs
    where 
        curDigit = (fromIntegral $ digitToInt c) :: Word128
        shiftAmount = (length cs) * 4