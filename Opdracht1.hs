module Opdracht1
where     
faca :: Int -> Int
faca 0 = 1                      -- opdr1A
faca x = x * faca(x-1)

facb :: Int -> Int
facb x 
    | x == 0 = 1                -- opdr1B
    | x > 0 = x * facb(x-1)
    | otherwise = error "Negative number"