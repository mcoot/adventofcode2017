module Day7 (day7Solution) where

import Solver

main :: IO ()
main = runSolver day7Solution "./data/day7.in"

test :: IO ()
test = runSolver day7Solution "./data/day7test.in"

day7Solution :: Solution String String
day7Solution = Solution {
    problemName = "Day 7",
    solParser   = Just . id,
    pt1Sol      = undefined,
    pt2Sol      = undefined
}