module Day3 (day3Solution) where

import Control.Monad.State
import Text.ParserCombinators.Parsec.Number

import Solver

-- Main

inputData :: Integer
inputData = 277678

main :: IO ()
main = runSolver day3Solution "./data/day3.in"

day3Solution :: Solution Integer Integer
day3Solution = Solution {
    problemName = "Day 3",
    solParser   = parseInputMaybe int,
    pt1Sol      = distanceToNum,
    pt2Sol      = (flip evalState [1]) . firstValLargerThan
}

-- Data

type Coord = (Integer, Integer)

data Move = LeftM Integer
          | RightM Integer
          | UpM Integer
          | DownM Integer
    deriving (Eq, Show)

-- Distance

manhattanDistance :: Coord -> Coord -> Integer
manhattanDistance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceToNum :: Integer -> Integer
distanceToNum = (flip manhattanDistance (0, 0)) . getCoordForNum

-- Spiral Coordinates

getCoordForNum :: Integer -> Coord
getCoordForNum = (performMovements (0, 0)) . countsToSpiralMoves . numToCounts

getNumForCoordHlpr :: Coord -> Integer -> Integer
getNumForCoordHlpr c i
    | getCoordForNum i == c = i
    | otherwise             = getNumForCoordHlpr c (i + 1)

getNumForCoord :: Coord -> Integer
getNumForCoord = flip getNumForCoordHlpr 1

performMovements :: Coord -> [Move] -> Coord
performMovements c []     = c
performMovements (x, y) (m:ms) = performMovements nxt ms
    where nxt = case m of
                    LeftM n  -> (x - n, y)
                    RightM n -> (x + n, y)
                    UpM n    -> (x, y + n)
                    DownM n  -> (x, y - n)

countSpiralHlpr :: [Integer] -> Integer -> [Move]
countSpiralHlpr [] _     = []
countSpiralHlpr (x:xs) i = m : (countSpiralHlpr xs (i + 1))
    where m = case mod i 4 of
                  0 -> RightM x
                  1 -> UpM x
                  2 -> LeftM x
                  3 -> DownM x

countsToSpiralMoves :: [Integer] -> [Move]
countsToSpiralMoves = (flip countSpiralHlpr 0)

countsHlpr :: Integer -> Integer -> [Integer]
countsHlpr 1 _    = []
countsHlpr n cnt  = c : (countsHlpr (n - c) (cnt + 1))
    where seqVal = quot cnt 2
          c      = if n - seqVal < 1 then
                       n - 1
                   else
                       seqVal

numToCounts :: Integer -> [Integer]
numToCounts = flip countsHlpr 2

-- Part 2

firstValLargerThan :: Integer -> State [Integer] Integer
firstValLargerThan n = do
    res <- valSearchStep
    if res > n then
        return res
    else do
        finalRes <- firstValLargerThan n
        return finalRes

valSearchStep :: State [Integer] Integer
valSearchStep = do
    vals <- get
    let curIndex = toInteger $ length vals + 1
    let newCell = sumSurroundingCells vals (getCoordForNum curIndex)
    put (vals ++ [newCell])
    return newCell


sumSurroundingCells :: [Integer] -> Coord -> Integer
sumSurroundingCells vals (x, y) = sum $ fmap (getCoordValue vals) [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), 
                                                                   (x, y - 1),                 (x, y + 1), 
                                                                   (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

getCoordValue :: [Integer] -> Coord -> Integer
getCoordValue vals coord = if (toInteger $ length vals) > (idx - 1) then
                                vals !! (fromIntegral idx - 1)
                            else
                                0
    where idx = getNumForCoord coord

{--
1   (0,   0)
2   (1,   0)
3   (1,   1)
4   (0,   1)
5   (-1,  1)
6   (-1,  0)
7   (-1, -1)
8   (0,  -1)
9   (1,  -1)
10  (2,  -1)

1 2 3 5 7 10 13 17 21 26 31 37 43 50
 1 1 2 2 3  3  4  4  5  5  6  6  7
 
R U LL DD RRR UUU LLLL DDDD RRRRR UUUUU
 --}