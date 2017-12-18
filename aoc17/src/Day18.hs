module Day18 (day18Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Data.Char
import Data.List

import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day18Solution "./data/day18.in"

test :: IO ()
test = runSolver day18Solution "./data/day18test.in"

day18Solution :: Solution Program Int
day18Solution = Solution {
    problemName = "Day 18",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = undefined,
    pt2Sol      = undefined
}

-- Solution

-- Data

type Program = [Instruction]

type RegName = Char

type Registers = Map RegName Int

data Value = ConstValue Int | RegValue RegName

data Instruction = ISound Value
                 | ISet RegName Value
                 | IAdd RegName Value
                 | IMul RegName Value
                 | IMod RegName Value
                 | IRcv Value
                 | IJump Value Value

-- Input parsing

inputParser :: Parser Program
inputParser = instructionParser `sepBy` endOfLine

instructionParser :: Parser Instruction
instructionParser =  (try $ ISound <$> (string "snd " >> valueParser))
                 <|> (try $ ISet   <$> (string "set " >> alphaNum) <*> valueParser)
                 <|> (try $ IAdd   <$> (string "add " >> alphaNum) <*> valueParser)
                 <|> (try $ IMul   <$> (string "mul " >> alphaNum) <*> valueParser)
                 <|> (try $ IMod   <$> (string "mod " >> alphaNum) <*> valueParser)
                 <|> (try $ IRcv   <$> (string "rcv " >> valueParser))
                 <|> (try $ IJump  <$> (string "jgz " >> valueParser) <*> valueParser)

valueParser :: Parser Value
valueParser =  (ConstValue <$> int)
           <|> (RegValue <$> alphaNum)
