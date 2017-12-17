module Day17 (day17Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Data.Char
import Data.List

import Control.Monad
import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as S

import Data.Vector (Vector)
import qualified Data.Vector as V

import Debug.Trace

import Solver

main :: IO ()
main = runSolverString day17Solution "377"

test :: IO ()
test = runSolverString day17Solution "3"

day17Solution :: Solution Int Int
day17Solution = Solution {
    problemName = "Day 17",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = getNextValueAfterInsertions 2017,
    pt2Sol      = valueAfterZeroAfterInsertions (50 * (10^6))
}

-- Solution

getNextValueAfterInsertions :: Int -> Int -> Int
getNextValueAfterInsertions num steps = (snd finalState) V.! ((fst finalState + 1) `rem` (length $ snd finalState))
    where
        finalState = execState (repeatInsertions num steps) (0, V.fromList [0])

valueAfterZeroAfterInsertions :: Int -> Int -> Int
valueAfterZeroAfterInsertions num steps = simulateAfterZero num 0 0 steps 0

simulateAfterZero :: Int -> Int -> Int -> Int -> Int -> Int
simulateAfterZero num iter idx steps found
    | iter >= num       = found
    | insertionPos == 1 = simulateAfterZero num (iter + 1) insertionPos steps (iter + 1)
    | otherwise         = simulateAfterZero num (iter + 1) insertionPos steps found
    where
        insertionPos = (idx + steps) `rem` (iter + 1) + 1

repeatInsertions :: Int -> Int -> State (Int, Vector Int) ()
repeatInsertions num steps = replicateM_ num (performInsertion steps)

performInsertion :: Int -> State (Int, Vector Int) ()
performInsertion steps = do
    (idx, v) <- get
    let insertionPos = (idx + steps) `rem` (length v) + 1
    let (before, after) = V.splitAt insertionPos v
    let newVec = before V.++ ((length v) `V.cons` V.empty) V.++ after
    put $ (insertionPos, newVec)    

-- Input parsing

inputParser :: Parser Int
inputParser = int