module Opdracht1 where
import Data.Bits  
-- opdr1a
faca :: Int -> Int
faca 0 = 1                    
faca x = x * faca (x - 1)

-- opdr1b
facb :: Int -> Int
facb x
    | x <= 0 = 1
    | otherwise = (facb (x-1)) * x

-- opdr2a
nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c = [(-1 * b + sqrt(b^2 +(-4 * a * c))) / (2 * a),(-1 * b - sqrt(b^2 +(-4 * a * c))) / (2 * a)]

-- opdr2b
nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
    | discriminant > 0 = [x1, x2]
    | discriminant == 0 = [x1]
    | otherwise = []
    where
        discriminant = b^2 - 4 * a * c
        x1 = (-b + sqrt(discriminant)) / (2 * a)
        x2 = (-b - sqrt(discriminant)) / (2 * a)

-- opdr2c
dobbel3 :: [(Integer, Integer, Integer)]
dobbel3 = [(x, y, z) | x <- [1..6], y <-[1..6], z <- [1..6], (x + y + z) `mod` 5 == 0] 

-- opdr2d
dobbel3n :: Integer -> [(Integer, Integer, Integer)]
dobbel3n n = [(x, y, z) | x <- [1..6], y <- [1..6], z <- [1..6], (x + y + z) `mod` n == 0] 

-- opdr3
puzzel :: [(Integer, Integer, Integer)]
puzzel = [(a,b,c) | a <- [-100..100], b <- [-100..100], c <- [-100..100], a == 2 * (b - c), b == a * c ,c * 2 == a + c] 

-- opdr4a
mult :: Integer -> Integer -> Integer
mult x 0 = 0
mult x y = x + mult x (y -1 )

{-
ghci> mult 999999999 99930819041
*** Exception: stack overflow
-}

-- opdr4b
fastmult :: Integer -> Integer -> Integer
fastmult x y
    | y == 0 = 0
    | even y = c
    | otherwise = x + c
    where c = fastmult (shiftL x 1) (shiftR y 1)

{-
ghci> fastmult 999999999 99930819041
99930818941069180959
-}

-- opdr5a
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x p = x * pow x (p-1)

-- opdr5b
