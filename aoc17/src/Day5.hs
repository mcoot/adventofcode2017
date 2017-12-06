module Day5 where

import Data.List
import Control.Monad.State

import qualified Data.Vector as V

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

data Execution = Execution {instructions :: V.Vector Integer, programCounter :: Integer}
    deriving (Show)

testInput :: [Integer]
testInput = [0, 3, 0, 1, -3]

main :: IO ()
main = do
    inputData <- parseFromFile parserInput "./data/day5.in"
    case inputData of
        Left _ -> putStrLn "Parsing failed"
        Right d -> do
            -- putStrLn $ show $ sum d
            putStrLn $ "Part 1: It will take " ++ (show $ numStepsToEscapeNoState d (+1)) ++ " steps to escape"
            putStrLn $ "Part 2: It will take " ++ (show $ numStepsToEscapeNoState d pt2UpdateFn) ++ " steps to escape"

-- Input parsing

parserInput :: Parser [Integer]
parserInput = int `sepBy` endOfLine

-- Implementation without using State

numStepsToEscapeNoState :: [Integer] -> (Integer -> Integer) -> Integer
numStepsToEscapeNoState prog updateFn = runProgram updateFn (Execution (V.fromList prog) 0)

runProgram :: (Integer -> Integer) -> Execution -> Integer
runProgram updateFn = toInteger . length . unfoldr (runStep updateFn)

runStep :: (Integer -> Integer) -> Execution -> Maybe (Integer, Execution)
runStep updateFn execution
    | pos < 0 || pos > (toInteger (V.length instrs - 1)) = Nothing
    | otherwise = Just $ (1, Execution newInstrs newPos)
    where pos = programCounter execution
          instrs = instructions execution
          curInstr = toInteger $ instrs V.! (fromIntegral pos)
          newPos = pos + curInstr
          newInstrs = instrs V.// [(fromIntegral pos, updateFn curInstr)]

pt2UpdateFn :: Integer -> Integer
pt2UpdateFn val = if (val >= 3) then
                    val - 1
                else
                    val + 1

-- Old implementation that hangs lol

numStepsToEscape :: [Integer] -> (Integer -> Integer) -> Integer
numStepsToEscape prog updateFn = evalState (executeProgram 0 updateFn) (Execution (V.fromList prog) 0)

executeProgram :: Integer -> (Integer -> Integer) -> State Execution Integer
executeProgram steps updateFn = do
    execution <- get
    let instrs = instructions execution
    let pos = programCounter execution
    n <- executeStep updateFn
    if n == 0 then
        return steps
    else
        executeProgram (steps + n) updateFn

executeStep :: (Integer -> Integer) -> State Execution Integer
executeStep updateFn = do
    execution <- get
    let instrs = instructions execution
    let pos = programCounter execution
    let curInstr = toInteger $ instrs V.! (fromIntegral pos)
    if pos < 0 || pos > (toInteger (V.length instrs - 1)) then
        -- Program is done, don't take a step
        return 0
    else do
        -- Perform step
        let newPos = pos + curInstr
        let newInstrs = instrs V.// [(fromIntegral pos, updateFn curInstr)]
        put $ Execution newInstrs newPos
        return 1