module Day2 (day2Solution) where

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

import Solver

testCase = [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]]

main :: IO ()
main = runSolver day2Solution "./data/day2.in"

day2Solution :: Solution [[Integer]] Integer
day2Solution = Solution {
    problemName = "Day 2",
    solParser   = parseInputMaybe parserInput,
    pt1Sol      = checksum1,
    pt2Sol      = checksum2
}

-- Parsing

parserRow :: Parser [Integer]
parserRow = many ((skipMany (tab <|> (char ' '))) *> int)

parserInput :: Parser [[Integer]]
parserInput = parserRow `sepBy` endOfLine
    
-- Evaluation

getMinAndMax :: [Integer] -> (Integer, Integer)
getMinAndMax row = (maximum row, minimum row)

cartProd :: [Integer] -> [(Integer, Integer)]
cartProd xs = (,) <$> xs <*> xs

getDivisPair :: [Integer] -> (Integer, Integer)
getDivisPair row = (flip (!!) 0) $ filter (\t -> (fst t /= snd t) && (fst t `rem` snd t == 0)) (cartProd row) 

checksum1 :: [[Integer]] -> Integer
checksum1 rows = sum $ ((\t -> fst t - snd t) . getMinAndMax) <$> rows

checksum2 :: [[Integer]] -> Integer
checksum2 rows = sum $ ((\t -> fst t `quot` snd t) . getDivisPair) <$> rows