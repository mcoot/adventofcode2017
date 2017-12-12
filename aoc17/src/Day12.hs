module Day12 (day12Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Data.Set (Set)
import qualified Data.Set as S

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.Query.BFS

import Control.Monad.State

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day12Solution "./data/day12.in"

test :: IO ()
test = runSolver day12Solution "./data/day12test.in"

day12Solution :: Solution (Gr Int ()) Int
day12Solution = Solution {
    problemName = "Day 12",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = S.size . (findNodesInGroup 1),
    pt2Sol      = S.size . (findGroups)
}

-- Solution

findNodesInGroup :: Int -> Gr Int () -> Set Int
findNodesInGroup num = S.fromList . (bfs num)

findGroups :: Gr Int () -> Set (Set Int)
findGroups g = S.fromList ((flip findNodesInGroup g) <$> nodes g)

-- Input parsing

inputParser :: Parser (Gr Int ())
inputParser = buildInputGraph <$> lineParser `sepBy` (endOfLine)

lineParser :: Parser (Int, [Int])
lineParser = do
    n1 <- int
    string " <-> "
    n2 <- int `sepBy` (char ',' *> spaces)
    return (n1, n2)

buildInputGraph :: [(Int, [Int])] -> Gr Int ()
buildInputGraph inp = run_ empty $ do
    insMapNodesM $ fst <$> inp
    insMapEdgesM $ concat $ getNeededEdges <$> inp

-- Undirected graph, so need edges both ways
getNeededEdges :: (Int, [Int]) -> [(Int, Int, ())]
getNeededEdges (from, tos) = concat $ (<$> tos) (\to -> [(from, to, ()), (to, from, ())])