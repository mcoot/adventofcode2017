module Day2 where

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number

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

-- Main

testCase = [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]]

main :: IO ()
main = do
    inputData <- parseFromFile parserInput "./data/day2.in"
    case inputData of
        Left _   -> putStrLn "Parsing failed"
        Right rows -> do
            putStrLn $ "Part 1 Checksum is " ++ (show $ checksum1 rows)
            putStrLn $ "Part 2 Checksum is " ++ (show $ checksum2 rows)