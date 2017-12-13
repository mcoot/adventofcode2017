module Day13 (day13Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Data.List

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day13Solution "./data/day13.in"

test :: IO ()
test = runSolver day13Solution "./data/day13test.in"

day13Solution :: Solution Firewall Int
day13Solution = Solution {
    problemName = "Day 13",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = getSeverityForRun,
    pt2Sol      = getMinimumDelay
}

-- Part 2

-- Find first delay such that for each depth, at (delay + depth) the scanner is not at position 0
-- Each scanner has an interval of period = 2 * (range - 1); so on step 0, period, 2 * period, 3 * period ... you will be caught
-- for example: delay d s.t. (d % 4 != 0) && ((d + 1) % 2 != 0) && ((d + 4) % 6 != 0)

getMinimumDelay :: Firewall -> Delay
getMinimumDelay f = hlpr f 0
    where hlpr f i = if willBeCaught f i then
                           i
                       else
                           hlpr f (i + 1)

          
willBeCaught :: Firewall -> Delay -> Bool
willBeCaught f d = all (/=0) $ (getPositionDelay f d) <$> M.keys f

getPositionDelay :: Firewall -> Delay -> Depth -> Position
getPositionDelay f delay depth = (delay + depth) `mod` period
    where
        range = f M.! depth
        period = 2 * (range - 1)

-- Part 1

getSeverityForRun :: Firewall -> Severity
getSeverityForRun f = severity f $ whereCaught $ executeRun f (initialScanners f)

severity :: Firewall -> [Depth] -> Severity
severity f [] = 0
severity f (x:xs) = ((if M.member x f then f M.! x else 0) * x) + severity f xs

whereCaught :: [Bool] -> [Depth]
whereCaught = wcHelper 0
    where
        wcHelper idx lst
            | idx >= length lst = []
            | otherwise         = if lst !! idx then idx : (wcHelper (idx + 1) lst) else wcHelper (idx + 1) lst

executeRun :: Firewall -> ScannerPositions -> [Bool]
executeRun f s = unfoldr (executeStep f (getMaxDepth f)) (s, 0)

executeStep :: Firewall -> Depth -> (ScannerPositions, Depth) -> Maybe (Bool, (ScannerPositions, Depth))
executeStep f maxDepth (s, d) = if d > maxDepth then
                                    Nothing
                                else
                                    Just (isCaught s (d, 0), (stepScanners f s, d + 1))

isCaught :: ScannerPositions -> (Depth, Position) -> Bool
isCaught s (d, pos) = if M.member d s then 
                          fst (s M.! d) == pos
                      else 
                          False

getMaxDepth :: Firewall -> Depth
getMaxDepth f = fst $ M.findMax f

initialScanners :: Firewall -> ScannerPositions
initialScanners f = M.fromList $ (\(k, a) -> (k, (0, Down))) <$> (M.toList f)

stepScanners :: Firewall -> ScannerPositions -> ScannerPositions
stepScanners f s = M.fromList $ (\d -> (d, getNextScannerPos f s d)) <$> (M.keys f)

getNextScannerPos :: Firewall -> ScannerPositions -> Depth -> (Position, Direction)
getNextScannerPos f s d
    | nextDir == Up = (curPos - 1, nextDir)
    | otherwise     = (curPos + 1, nextDir)
    where
        (curPos, curDir) = s M.! d
        curDepth = f M.! d
        nextDir = case curDir of
                      Up   -> if curPos <= 0 then Down else Up
                      Down -> if curPos >= curDepth - 1 then Up else Down

-- Data

type Depth = Int
type Range = Int
type Position = Int
type Severity = Int
type Delay = Int

data Direction = Up | Down deriving (Eq, Show)

type Firewall = Map Depth Range

type ScannerPositions = Map Depth (Position, Direction)

-- Input parsing

inputParser :: Parser Firewall
inputParser = do
    layers <- layerParser `sepBy` endOfLine
    return $ M.fromList layers

layerParser :: Parser (Depth, Range) 
layerParser = (,) <$> int <*> (char ':' *> spaces *> int)