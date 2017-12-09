module Day8 (day8Solution) where

import Text.Parsec hiding (State)
-- import Text.Parsec.Combinator
-- import Text.Parsec.String (Parser, parseFromFile)
-- import Text.ParserCombinators.Parsec.Number
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.ParserCombinators.Parsec.Number

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Solver

main :: IO ()
main = runSolver day8Solution "./data/day8.in"

test :: IO ()
test = runSolver day8Solution "./data/day8test.in"

testExec :: IO ()
testExec = runSolver day8ExecutionSol "./data/day8test.in"

day8Solution :: Solution [Instruction] Integer
day8Solution = Solution {
    problemName = "Day 8",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = getMaxRegisterValue . execProgram,
    pt2Sol      = fst . execProgramKeepingMax
}

day8ExecutionSol :: Solution [Instruction] (Map Register Integer)
day8ExecutionSol = Solution {
    problemName = "Day 8 Execution",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = execProgram,
    pt2Sol      = execProgram
}

-- Part 1

getMaxRegisterValue :: Map Register Integer -> Integer
getMaxRegisterValue m = (m Map.!) $ getMaxRegister m

getMaxRegister :: Map Register Integer -> Register
getMaxRegister m = maxReg True (Register "", 0) (Map.toList m)
    where 
        maxReg _ (br, _) []                 = br
        maxReg True _ ((r, v):rest)         = maxReg False (r, v) rest
        maxReg False (br, bv) ((r, v):rest) 
            | v > bv                        = maxReg False (r, v) rest
            | otherwise                     = maxReg False (br, bv) rest


-- Part 2

execInstructionKeepingMax :: (Integer, Map Register Integer) -> Instruction -> (Integer, Map Register Integer)
execInstructionKeepingMax (best, m) instr = (max curMax best, newM)
    where
        newM   = execInstruction m instr
        curMax = getMaxRegisterValue newM

execProgramKeepingMax :: [Instruction] -> (Integer, Map Register Integer)
execProgramKeepingMax = foldl execInstructionKeepingMax (0, Map.empty)

-- Data

data Register = Register {regName :: String} deriving (Ord, Eq, Show)

data Op = Inc | Dec deriving (Eq, Show)

data Command = Command Register Op Integer deriving (Eq, Show)

data Relation = LessThan | LessThanEq | Equals | NotEquals | GreaterThanEq | GreaterThan
    deriving (Eq, Show)

data Condition = Condition Register Relation Integer deriving (Eq, Show)

data Instruction = Instruction Condition Command deriving (Eq, Show)

-- Input parsing

lang = emptyDef {
      identStart      = letter
    , identLetter     = alphaNum
    , opStart         = oneOf "<>=!id"
    , reservedOpNames = ["<", "<=", "==", "!=", ">=", ">", "inc", "dec", "if"]
}

lexer :: TokenParser ()
lexer = makeTokenParser lang

inputParser :: Parser [Instruction]
inputParser = instructionParser `sepBy` endOfLine

instructionParser :: Parser Instruction
instructionParser = do
    whiteSpace lexer
    com <- commandParser
    reservedOp lexer "if"
    cond <- conditionParser
    return $ Instruction cond com

commandParser :: Parser Command
commandParser = do
    reg <- registerParser
    op <- opParser
    val <- valueParser
    return $ Command reg op val

registerParser :: Parser Register
registerParser = identifier lexer >>= (return . Register)

valueParser :: Integral i => Parser i
valueParser = do
    v <- int
    skipMany $ char ' '
    return v

opParser :: Parser Op
opParser =  (reservedOp lexer "inc" >> return Inc)
        <|> (reservedOp lexer "dec" >> return Dec)

conditionParser :: Parser Condition
conditionParser = do
    reg <- registerParser
    rel <- relationParser
    val <- valueParser
    return $ Condition reg rel val

relationParser :: Parser Relation
relationParser =  (reservedOp lexer "<=" >> return LessThanEq) 
              <|> (reservedOp lexer "<" >> return LessThan)
              <|> (reservedOp lexer "==" >> return Equals)
              <|> (reservedOp lexer "!=" >> return NotEquals)
              <|> (reservedOp lexer ">=" >> return GreaterThanEq)
              <|> (reservedOp lexer ">" >> return GreaterThan)

-- Execution

regGet :: Map Register Integer -> Register -> (Integer, Map Register Integer)
regGet m r = if Map.member r m then (m Map.! r, m) else (0, Map.insert r 0 m)

regAdd :: Map Register Integer -> Register -> Integer -> Map Register Integer
regAdd m r v = Map.insert r (rVal + v) newM
    where (rVal, newM) = regGet m r

regSub :: Map Register Integer -> Register -> Integer -> Map Register Integer
regSub m r v = Map.insert r (rVal - v) newM
    where (rVal, newM) = regGet m r

evalRel :: Integer -> Relation -> Integer -> Bool
evalRel a LessThan b      = a < b
evalRel a LessThanEq b    = a <= b
evalRel a Equals b        = a == b
evalRel a NotEquals b     = a /= b
evalRel a GreaterThanEq b = a >= b
evalRel a GreaterThan b   = a > b

evalCondition :: Map Register Integer -> Condition -> (Bool, Map Register Integer)
evalCondition m (Condition r rel val) = (evalRel rVal rel val, newM)
    where (rVal, newM) = regGet m r

execCommand :: Map Register Integer -> Command -> Map Register Integer
execCommand m (Command r Inc val) = regAdd m r val
execCommand m (Command r Dec val) = regSub m r val

execInstruction :: Map Register Integer -> Instruction -> Map Register Integer
execInstruction m (Instruction cond com) = if res then execCommand newM com else newM
    where
        e    = evalCondition m cond
        res  = fst e
        newM = snd e

execProgram :: [Instruction] -> Map Register Integer
execProgram = foldl execInstruction Map.empty