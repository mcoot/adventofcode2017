from typing import *

def execProgram(prog: List[int], updateFn) -> int:
    pos = 0
    steps = 0
    while (pos >= 0 and pos < len(prog)):
        nextPos = pos + prog[pos]
        prog[pos] = updateFn(prog[pos])
        pos = nextPos
        steps += 1
    return steps

def pt1Update(val: int) -> int:
    return val + 1

def pt2Update(val:int) -> int:
    return (val + 1) if val < 3 else (val - 1)

def readData(fname: str):
    with open(fname, 'r') as fl:
        lines = [int(l) for l in fl.read().splitlines()]
    return lines

testData = [0, 3, 0, 1, -3]

data = readData("./data/day5.in")
print("Calculating...")
print("Part 1: " + str(execProgram(data[:], pt1Update)))
print("Part 2: " + str(execProgram(data[:], pt2Update)))
