module Day11 (day11Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.State

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day11Solution "./data/day11.in"

test :: IO ()
test = runSolver day11Solution "./data/day11test.in"

day11Solution :: Solution [HexDirection] Int
day11Solution = Solution {
    problemName = "Day 11",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = getDistance . getCoord . reverse,
    pt2Sol      = getMaxDistance
}

-- Solution

part2 :: [HexDirection] -> Int
part2 dirs = 0

getMaxDistance :: [HexDirection] -> Int
getMaxDistance dirs = maximum dists
    where dists = getDistance <$> getCoordSteps (reverse dirs)

getDistance :: HexCoord -> Int
getDistance (x, y, z) = ((abs x) + (abs y) + (abs z)) `quot` 2

getCoordSteps :: [HexDirection] -> [HexCoord]
getCoordSteps [] = [(0, 0, 0)]
getCoordSteps (x:xs) = (curX + mX, curY + mY, curZ + mZ) : prev
    where
        prev               = getCoordSteps xs
        (curX, curY, curZ) = (flip (!!) 0) prev
        (mX, mY, mZ)       = moveStep x

getCoord :: [HexDirection] -> HexCoord
getCoord = (flip (!!) 0) . getCoordSteps

moveStep :: HexDirection -> HexCoord
moveStep N = (0, 1, -1)
moveStep NE = (1, 0, -1)
moveStep SE = (1, -1, 0)
moveStep S = (0, -1, 1)
moveStep SW = (-1, 0, 1)
moveStep NW = (-1, 1, 0)

-- Data

data HexDirection = N | NE | E | SE | S | SW | NW deriving (Eq, Show)

type HexCoord = (Int, Int, Int)

-- Input parsing

inputParser :: Parser [HexDirection]
inputParser = directionParser `sepBy` char ','

directionParser :: Parser HexDirection
directionParser =  (try $ string "ne" *> return NE)
               <|> (try $ string "nw" *> return NW)
               <|> (try $ string "se" *> return SE)
               <|> (try $ string "sw" *> return SW)
               <|> (string "n" *> return N)
               <|> (string "s" *> return S)