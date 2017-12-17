module Solver (Solution(Solution, problemName, solParser, pt1Sol, pt2Sol),
               runSolver, 
               runSolverString, 
               parseInputMaybe) where

import System.CPUTime
import Control.Exception
import Control.Monad

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

data Solution a b = Solution {problemName :: String, solParser :: String -> Maybe a, pt1Sol :: a -> b, pt2Sol :: a -> b}

time :: t -> IO (t, Double)
time a = do
    start <- getCPUTime
    res <- evaluate a
    end <- getCPUTime
    let ms = (fromIntegral $ end - start) / (10^9)
    return (res, ms)

solve :: Show b => Solution a b -> String -> IO ()
solve Solution {problemName = _,
                solParser = p, 
                pt1Sol = part1, 
                pt2Sol = part2} inputData = do
                    let parsed = p inputData
                    case parsed of
                        Nothing -> putStrLn "Failed to parse input"
                        Just d  -> do
                            (r1, t1) <- time $ part1 d
                            putStrLn $ "Part 1: " ++ (show r1) ++ " [" ++ (show t1) ++ "ms]"
                            (r2, t2) <- time $ part2 d
                            putStrLn $ "Part 2: " ++ (show r2) ++ " [" ++ (show t2) ++ "ms]"
    
runSolverString :: (Show b) => Solution a b -> String -> IO ()
runSolverString sol inputData = do
    putStrLn ("Solving " ++ problemName sol)
    solve sol inputData

runSolver :: (Show b) => Solution a b -> FilePath -> IO ()
runSolver sol src = readFile src >>= (runSolverString sol)

parseInputMaybe :: Parser a -> String -> Maybe a
parseInputMaybe p s = case parse p "" s of
                          Left _  -> Nothing
                          Right r -> Just r