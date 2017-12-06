module Day6 (day6Solution) where

import Text.Parsec hiding (State)
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Data.List

import Data.HashSet (Set)
import qualified Data.HashSet as Set

import Solver

testInput :: [Integer]
testInput = [0, 2, 7, 0]

main :: IO ()
main = runSolver day6Solution "./data/day6.in"

day6Solution :: Solution [Integer] Integer
day6Solution = Solution {
    problemName = "Day 6",
    solParser   = parseInputMaybe inputParser,
    pt1Sol      = stepsUntilFirstRepeat,
    pt2Sol      = stepsUntilSecondRepeat
}

-- Input parsing

inputParser :: Parser [Integer]
inputParser = many ((skipMany (tab <|> (char ' '))) *> int)

-- Data

data Config = Config {configCurrent :: [Integer], configHistory :: Set [Integer]} 
    deriving (Show, Eq)

buildInitialConfig :: [Integer] -> Config
buildInitialConfig config = Config config (Set.empty)

-- Part 1

getFirstRepeat = unfoldr makeStep . buildInitialConfig

stepsUntilFirstRepeat :: [Integer] -> Integer
stepsUntilFirstRepeat = toInteger . length . getFirstRepeat

makeNSteps :: Config -> Integer -> Maybe ([Integer], Config)
makeNSteps init 0 = Just $ (configCurrent init, init)
makeNSteps init n = do
    nxt <- makeStep init
    makeNSteps (snd $ nxt) (n - 1)

extractCfg :: Maybe (Integer, Config) -> Config
extractCfg (Just (_, cfg)) = cfg

makeStep :: Config -> Maybe ([Integer], Config)
makeStep Config {configCurrent = cur, configHistory = set}
    | Set.member cur set = Nothing
    | otherwise          = Just $ (nextCfg, Config nextCfg (Set.insert cur set))
    where nextCfg = redistribute (toInteger maxIdx) cur
          maxIdx  = getMaxIdx cur


getMaxIdx :: [Integer] -> Int
getMaxIdx lst = getMaxIdxHlpr lst 0 (-1, 0)

getMaxIdxHlpr :: [Integer] -> Integer -> (Int, Integer) -> Int
getMaxIdxHlpr lst idx (midx, m)
    | intIdx == 0          = getMaxIdxHlpr lst 1 (0, lst !! 0)
    | intIdx >= length lst = midx
    | otherwise            = if (lst !! intIdx) > m then
                                 getMaxIdxHlpr lst (idx + 1) (intIdx, lst !! intIdx)
                             else
                                 getMaxIdxHlpr lst (idx + 1) (midx, m)
    where intIdx = fromIntegral $ idx

redistribute :: Integer -> [Integer] -> [Integer]
redistribute maxIdx banks = fmap (redistributeElement maxIdx banks . toInteger) [0..length banks - 1] 

redistributeElement :: Integer -> [Integer] -> Integer -> Integer
redistributeElement maxIdx banks idx = carryOver + addedAmount
    where maxVal = toInteger $ banks !! (fromIntegral maxIdx)
          shiftIdx  = (fromIntegral $ idx - (maxIdx + 1)) `mod` (length banks)
          carryOver = if idx == (fromIntegral maxIdx) then 0 else banks !! (fromIntegral idx)
          addedAmount = maxVal `quot` (toInteger $ length banks) + if (toInteger shiftIdx) < maxVal `rem` (toInteger $ length banks) then 1 else 0

-- Part 2

stepsUntilSecondRepeat :: [Integer] -> Integer
stepsUntilSecondRepeat banks = stepsUntilFirstRepeat firstRepeat
    where toFirstRepeat = getFirstRepeat banks
          firstRepeat   = toFirstRepeat !! (length toFirstRepeat - 1)