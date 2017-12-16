module Day16 (day16Solution) where

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
main = runSolver day16Solution "./data/day16.in"

test :: IO ()
test = runSolver day16Solution "./data/day16test.in"

day16Solution :: Solution [DanceMove] String
day16Solution = Solution {
    problemName = "Day 16",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = V.toList . (performDance initialPositions),
    pt2Sol      = V.toList . (performDanceRepeatedly (10^9))
}

-- Solution

performDance :: Vector Char -> [DanceMove] -> Vector Char
performDance initial moves = execState (execMoveList moves) initial

performDanceRepeatedly :: Int -> [DanceMove] -> Vector Char
performDanceRepeatedly times moves = execState (replicateM_ neededIters $ execMoveList moves) initialPositions
    where 
        neededIters = getNecessaryIters times moves

getNecessaryIters :: Int -> [DanceMove] -> Int
getNecessaryIters totalIters moves = totalIters `rem` period
    where 
        period = findCycle moves S.empty initialPositions 0

findCycle :: [DanceMove] -> Set (Vector Char) -> Vector Char -> Int -> Int
findCycle moves seen curConfig iters  = if S.member nextConfig seen then
                                            iters
                                        else
                                            findCycle moves (S.insert nextConfig seen) nextConfig (iters + 1)
    where 
        nextConfig = performDance curConfig moves

initialPositions :: Vector Char
initialPositions = V.fromList ['a'..'p']

execMoveList :: [DanceMove] -> State (Vector Char) ()
execMoveList moves = forM_ moves execDanceMove

execDanceMove :: DanceMove -> State (Vector Char) ()
execDanceMove mv = do
    v <- get
    put $ performMove v mv
    return ()

performMove :: Vector Char -> DanceMove -> Vector Char
performMove v (Spin x) = toMove V.++ rest
    where 
        rest   = V.slice 0 (length v - x) v
        toMove = V.slice (length v - x) x v

performMove v (Exchange i j) = v V.// [(i, vj), (j, vi)]
    where 
        vi = v V.! i
        vj = v V.! j

performMove v (Partner vi vj) = v V.// [(i, vj), (j, vi)]
    where
        i = findIndexUnsafe vi v
        j = findIndexUnsafe vj v     

findIndexUnsafe :: Eq a => a -> Vector a -> Int
findIndexUnsafe elem v = case V.elemIndex elem v of
                             Nothing -> error "Index failed"
                             Just i  -> i


-- Data

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char deriving (Eq, Show)

-- Input parsing

inputParser :: Parser [DanceMove]
inputParser = parseDanceMove `sepBy` char ','

parseDanceMove :: Parser DanceMove
parseDanceMove =  parseSpin
              <|> parseExchange
              <|> parsePartner

parseSpin :: Parser DanceMove
parseSpin = do
    char 's'
    Spin <$> int

parseExchange :: Parser DanceMove
parseExchange = do
    char 'x'
    p1 <- int
    char '/'
    p2 <- int
    return $ Exchange p1 p2

parsePartner :: Parser DanceMove
parsePartner = do
    char 'p'
    p1 <- anyChar
    char '/'
    p2 <- anyChar
    return $ Partner p1 p2
    