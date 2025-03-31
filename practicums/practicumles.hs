module Practicumles 
    where

import Data.List
import Data.Char

fiveadder :: Num a => [a] -> [a]
fiveadder [] = []
fiveadder (x:xs) = x + 5 : fiveadder xs --recursieve aanroep


addn :: [Int] -> Int -> [Int]
addn [] _ = []
addn (x:xs) n = x + n : addn xs n

applyall :: (a -> b) -> [a] -> [b]
applyall _ [] = []
applyall f (x:xs) = f x : applyall f xs