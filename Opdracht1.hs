module Opdracht1
where     
import Distribution.Simple.Utils (xargs)
faca :: Int -> Int
faca 0 = 1                      -- opdr1A Pattern matching
faca x = x * faca(x-1)

facb :: Int -> Int
facb x 
    | x == 0 = 1                -- opdr1B Guards
    | x > 0 = x * facb(x-1)
    | otherwise = error "Negative number"

nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c
    | d < 0 = []                -- opdr2A Guards
    | d == 0 = [-b / (2 * a)]
    | otherwise = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
    where d = b * b - 4 * a * c

nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
    | d < 0 = []                -- opdr2B Guards
    | d == 0 = [x1]
    | otherwise = [x1, x2]
    where
        d  = b * b - 4 * a * c
        x1 = (-b + sqrt d) / (2 * a)
        x2 = (-b - sqrt d) / (2 * a)

dobbel2 = [[x,y]|x<-[1..6],y<-[1..6]] -- opdr2C
dobbel3 = [[x,y,x+y]|x<-[1..6],y<-[1..6]]