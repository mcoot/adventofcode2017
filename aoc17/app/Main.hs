module Main where

import Solver

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16

main :: IO ()
main = do
    putStrLn $ "Advent of Code 2017 Solutions"
    runSolver day1Solution "../src/data/day1.in"
    runSolver day2Solution "../src/data/day2.in"
    runSolver day3Solution "../src/data/day3.in"
    runSolver day4Solution "../src/data/day4.in"
    runSolver day5Solution "../src/data/day5.in"
    runSolver day6Solution "../src/data/day6.in"
    runSolver day8Solution "../src/data/day8.in"
    runSolver day9Solution "../src/data/day9.in"
    runSolver day10Solution "../src/data/day10.in"
    runSolver day11Solution "../src/data/day11.in"
    runSolver day12Solution "../src/data/day12.in"
    runSolver day13Solution "../src/data/day13.in"
    runSolver day14Solution "../src/data/day14.in"
    runSolver day15Solution "../src/data/day15.in"
    runSolver day16Solution "../src/data/day16.in"