module Day9 (day9Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Control.Monad
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day9Solution "./data/day9.in"

test :: IO ()
test = runSolver day9Solution "./data/day9test.in"

day9Solution :: Solution Group Integer
day9Solution = Solution {
    problemName = "Day 9",
    solParser   = parseInputMaybe groupParser,
    pt1Sol      = (flip getGroupScore 1),
    pt2Sol      = getGarbageCount
}

-- Part 1

getGroupScore :: Group -> Integer -> Integer
getGroupScore (Group []) depth = depth
getGroupScore (Group elems) depth = depth + (sum $ gpElemScore <$> elems)
    where 
        gpElemScore elem = case elem of
                               GroupElement gp  -> (getGroupScore gp (depth + 1))
                               GarbageElement _ -> 0

getGarbageCount :: Group -> Integer
getGarbageCount (Group []) = 0
getGarbageCount (Group elems) = sum $ garbElemScore <$> elems
    where
        garbElemScore elem = case elem of
                                 GroupElement gp            -> getGarbageCount gp
                                 GarbageElement (Garbage s) -> toInteger $ length s

-- Part 2

-- Data

data Group = Group [Element] deriving (Show, Eq)

data Garbage = Garbage String deriving (Show, Eq)

data Element = GroupElement Group | GarbageElement Garbage 
    deriving (Show, Eq)

-- Input parsing

groupParser :: Parser Group
groupParser = Group <$> between (char '{') (char '}') (elementParser `sepBy` (char ','))

elementParser :: Parser Element
elementParser =  try (GroupElement <$> groupParser)
             <|> (GarbageElement <$> garbageParser)

garbageParser :: Parser Garbage
garbageParser = Garbage <$> between (char '<') (char '>') (concat <$> many garbageCharParser)

garbageCharParser :: Parser String
garbageCharParser =  try $ (char '!' *> anyChar *> (return ""))
                 <|> ((:[]) <$> (noneOf ">"))