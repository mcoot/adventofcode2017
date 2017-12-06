module Main where

import Solver

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

main :: IO ()
main = do
    putStrLn $ "Advent of Code 2017 Solutions"
    runSolver day1Solution "../src/data/day1.in"
    runSolver day2Solution "../src/data/day2.in"
    runSolver day3Solution "../src/data/day3.in"
    runSolver day4Solution "../src/data/day4.in"
    runSolver day5Solution "../src/data/day5.in"
    runSolver day6Solution "../src/data/day6.in"