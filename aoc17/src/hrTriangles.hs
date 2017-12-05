module Triangles where

import Control.Monad.State

duplicate :: [a] -> Integer -> [a]
duplicate s n = concat $ replicate (fromIntegral n) s

slice :: [a] -> Integer -> Integer -> [a]
slice s f t = take (ti - fi) (drop fi s) 
    where ti = fromIntegral t
          fi = fromIntegral f

getTriangleHeight :: Integer -> Integer
getTriangleHeight 0 = 32
getTriangleHeight n = (getTriangleHeight $ n - 1) `quot` 2

getTriangleWidth :: Integer -> Integer
getTriangleWidth 0 = 63
getTriangleWidth n = (((getTriangleHeight (n - 1)) + 1) `quot` 2) - 1

baseTriangleRow :: Integer -> String
baseTriangleRow row = (duplicate "_" uc) ++ (duplicate "1" c) ++ (duplicate "_" uc) 
    where c  = row * 2 - 1
          uc = (getTriangleWidth 0 - c) `quot` 2

baseTriangle :: [String]
baseTriangle = fmap baseTriangleRow [1..(getTriangleHeight 0)]

carveTriangleRow :: [String] -> ((Integer, Integer), (Integer, Integer)) -> Integer -> String
carveTriangleRow orig ((xLeft, yTop), (xRight, yBottom)) row
    | (row - 1) < carveStartRow || row > yBottom = origRow
    | otherwise = (slice origRow 0 (xLeft + carvePadding + carveRowIndex - 1))
                  ++ (duplicate "_" carveAmount)
                  ++ (slice origRow (xRight - carvePadding - carveRowIndex + 1) tWidth)
    where origRow = orig !! (fromIntegral row - 1)
          tHeight = toInteger $ length orig
          tWidth = toInteger $ length origRow
          cHeight = yBottom - yTop
          cWidth = xRight - xLeft
          carveStartRow = yTop + (cHeight `quot` 2)
          carveRowIndex = row - carveStartRow
          carvePadding = (cWidth + 1) `quot` 4
          carveAmount = (cWidth + 1) `quot` 2 + 1 - 2 * carveRowIndex          

carveTriangle :: [String] -> (Integer, Integer) -> Integer -> [String]
carveTriangle orig (x, y) scale = fmap (carveTriangleRow orig ((x, y), (x + width, y + height))) [1 .. (toInteger $ length orig)]
    where height = (getTriangleHeight scale)
          width  = (getTriangleWidth scale)

getCarveStepPos :: Integer -> Integer -> (Integer, Integer)
-- getCarveStepPos idx scale = (0, 0)
getCarveStepPos idx 0 = (0, 0)
getCarveStepPos 1 1 = (16, 0)
getCarveStepPos 2 1 = (0, 16)
getCarveStepPos 3 1 = (32, 0)

-- (0, 0) | (16, 0), (0, 16), (32, 16)

carveFractalIteration :: Integer -> State [String] ()
carveFractalIteration scale = do
    orig <- get
    put $ carveTriangle orig (getCarveStepPos 1 scale) scale
    -- forM_ [1..(3^scale)] $ \idx -> do
    --                               orig <- get
    --                               put $ carveTriangle orig (getCarveStepPos idx scale) scale

carveFractalRecurse :: Integer -> Integer -> State [String] ()
carveFractalRecurse toIter idx
    | idx > toIter = return ()
    | otherwise = do
        carveFractalIteration idx
        carveFractalRecurse toIter (idx + 1)

carveFractal :: Integer -> State [String] ()
carveFractal toIter = carveFractalRecurse toIter 0

-- 0 1 3 9

genFractal :: Integer -> [String]
genFractal n = execState (carveFractal (n - 1)) baseTriangle --carveTriangle (genFractal 0) (0, 0) 0

triangleRepr :: [String] -> String
triangleRepr = foldr (\cur acc -> cur ++ "\n" ++ acc) ""

genFractalRepr :: Integer -> String
genFractalRepr iter = triangleRepr $ genFractal iter


main :: IO ()
main = do
    num <- readLn :: IO Integer
    putStrLn $ genFractalRepr num