module Day4 where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    inputData <- readFile "./data/day4.in"
    putStrLn $ "Part 1: the input had " ++ (show $ countValidPassPhrases pt1Checker $ lines inputData) ++ " valid passphrases"
    putStrLn $ "Part 2: the input had " ++ (show $ countValidPassPhrases pt2Checker $ lines inputData) ++ " valid passphrases"

countValidPassPhrases :: (String -> Bool) -> [String] -> Integer
countValidPassPhrases f phrases = foldr (\cur acc -> acc + (if f cur then 1 else 0)) 0 phrases

-- Part 1

pt1CheckerRecurse :: Set String -> [String] -> Bool
pt1CheckerRecurse _ []     = True
pt1CheckerRecurse s (x:xs) = if Set.member x s then
                                        False
                                    else
                                        pt1CheckerRecurse (Set.insert x s) xs

pt1Checker :: String -> Bool
pt1Checker phrase = pt1CheckerRecurse Set.empty $ words phrase

-- Part 2

makeCharRepr :: [Char] -> Map Char Integer
makeCharRepr []     = Map.empty
makeCharRepr (x:xs) = Map.insert x ((Map.findWithDefault 0 x m) + 1) m
    where m = makeCharRepr xs

pt2CheckerRecurse :: Set (Map Char Integer) -> [String] -> Bool
pt2CheckerRecurse _ []     = True
pt2CheckerRecurse s (x:xs) = if Set.member xMap s then
                                False
                            else
                                pt2CheckerRecurse (Set.insert xMap s) xs
    where
        xMap = makeCharRepr x


pt2Checker :: String -> Bool
pt2Checker phrase = pt2CheckerRecurse Set.empty $ words phrase