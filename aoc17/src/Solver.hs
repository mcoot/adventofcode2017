module Solver where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

data Solution a b = Solution {problemName :: String, solParser :: String -> Maybe a, pt1Sol :: a -> b, pt2Sol :: a -> b}

solve :: Solution a b -> FilePath -> IO (Maybe (b, b))
solve Solution {problemName = _,
                solParser = p, 
                pt1Sol = part1, 
                pt2Sol = part2} src = do
                    inputData <- readFile src
                    let parsed = p inputData
                    case parsed of
                        Nothing -> return Nothing
                        Just d  -> return $ Just (part1 d, part2 d)

showSolved :: Show b => Maybe (b, b) -> IO ()
showSolved res = case res of
                    Nothing       -> putStrLn "Failed to parse input"
                    Just (r1, r2) -> do
                        putStrLn $ "Part 1: " ++ (show r1)
                        putStrLn $ "Part 2: " ++ (show r2)


runSolver :: (Show b) => Solution a b -> FilePath -> IO ()
runSolver sol src = putStrLn ("Solving " ++ problemName sol) >> solve sol src >>= showSolved

parseInputMaybe :: Parser a -> String -> Maybe a
parseInputMaybe p s = case parse p "" s of
                          Left _  -> Nothing
                          Right r -> Just r