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
    pt1Sol      = const 1,
    pt2Sol      = solvePart2
}

-- Solution

solvePart2 :: Program -> Int
solvePart2 = undefined

execInstruction :: Instruction -> ProgID -> State Execution Bool
execInstruction (ISet r v) prog = performRegisterMath (\_ b -> b) prog r v
execInstruction (IAdd r v) prog = performRegisterMath (+) prog r v
execInstruction (IMul r v) prog = performRegisterMath (*) prog r v
execInstruction (IMod r v) prog = performRegisterMath mod prog r v
execInstruction (IJump x y) prog = do
    ex <- get
    let ExecState m q pc n = getProgState prog ex
    if getValue x m <= 0 then return False
    else do
        put $ updateProgState prog m q (pc + getValue y m) n ex
        return False
execInstruction (ISnd v) prog = do
    return False
execInstruction (IRcv v) prog = do
    return False

performRegisterMath :: (Int -> Int -> Int) -> ProgID -> RegName -> Value -> State Execution Bool
performRegisterMath op prog r v = do
    ex <- get
    let ExecState m q pc n = getProgState prog ex
    let newVal = (accessRegister r m) `op` (getValue v m)
    let newM = M.insert r newVal m
    put $ updateProgState prog newM q (pc + 1) n ex
    return False

-- Data

data Execution = Execution {getProgram :: Program, getProg0State :: ExecState, getProg1State :: ExecState}

getProgState :: ProgID -> Execution -> ExecState
getProgState Prog0 = getProg0State
getProgState Prog1 = getProg1State

updateProgState :: ProgID -> Registers -> MessageQueue -> Int -> Int -> Execution -> Execution
updateProgState Prog0 m q pc n ex = Execution (getProgram ex) (ExecState m q pc n) (getProg1State ex)
updateProgState Prog1 m q pc n ex = Execution (getProgram ex) (getProg0State ex) (ExecState m q pc n)

data ExecState = ExecState {getRegisters :: Registers, getMessageQueue :: MessageQueue, getProgramCounter :: Int, getNumSends :: Int}

type ProgramState = (Registers, MessageQueue, Int)

type SendCounts = (Int, Int)

data ProgID = Prog0 | Prog1 deriving (Eq, Show)

type MessageQueue = [Int]

type Program = [Instruction]

type RegName = Char

type Registers = Map RegName Int

data Value = ConstValue Int | RegValue RegName deriving (Eq, Show)

accessRegister :: RegName -> Registers -> Int
accessRegister = M.findWithDefault 0

getValue :: Value -> Registers -> Int
getValue (ConstValue x) _ = x
getValue (RegValue r) m = accessRegister r m

data Instruction = ISnd Value
                 | ISet RegName Value
                 | IAdd RegName Value
                 | IMul RegName Value
                 | IMod RegName Value
                 | IRcv Value
                 | IJump Value Value
    deriving (Eq, Show)

fstTriple :: (a, b, c) -> a
fstTriple (a, _, _) = a

sndTriple :: (a, b, c) -> b
sndTriple (_, b, _) = b

thdTriple :: (a, b, c) -> c
thdTriple (_, _, c) = c

-- Input parsing

inputParser :: Parser Program
inputParser = instructionParser `sepBy` endOfLine

instructionParser :: Parser Instruction
instructionParser =  (try $ ISnd <$> (string "snd " >> valueParser))
                 <|> (try $ ISet   <$> (string "set " >> alphaNum) <*> valueParser)
                 <|> (try $ IAdd   <$> (string "add " >> alphaNum) <*> valueParser)
                 <|> (try $ IMul   <$> (string "mul " >> alphaNum) <*> valueParser)
                 <|> (try $ IMod   <$> (string "mod " >> alphaNum) <*> valueParser)
                 <|> (try $ IRcv   <$> (string "rcv " >> valueParser))
                 <|> (try $ IJump  <$> (string "jgz " >> valueParser) <*> valueParser)

valueParser :: Parser Value
valueParser =  (ConstValue <$> int)
           <|> (RegValue <$> alphaNum)
